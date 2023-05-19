namespace aleph.qpu.qsharp

open System

open aleph.utils
open aleph.kets

open Microsoft.Quantum.Simulation.Core
open aleph.qsharp

type QuantumState = universe.Universe
type QuantumValue = value.Value
type QuantumRegister = register.Register

type RegistersMap = Map<KetId, QuantumRegister>

type Universe(sim: IOperationFactory, state: QuantumState, allocations: RegistersMap) =
    let mutable value = None

    interface IUniverse

    member this.Sample(kets: Ket list) =
        let registers = kets |> List.map (fun k -> allocations.[k.Id])
        match value with
        | Some v -> v
        | None ->
            let sample =
                universe.Sample.Run(sim, state, registers |> QArray).Result
                |> Seq.map (fun i -> i.value|> int)
                |> Seq.toList

            value <- Some sample
            sample

    override this.ToString() =
        universe.Print.Run(sim, state).Result |> ignore
        "[see above]"

type QsharpContext =
    { state: QuantumState
      allocations: RegistersMap }

type Processor(sim: IOperationFactory) =

    let toQValue (i: int) = 
        let w = int_width i |> int64
        new QuantumValue((i |> int64, w))

    let rec prepare ctx (ket: Ket) =
        match ctx.allocations.TryFind ket.Id with
        | Some _ -> ctx |> Ok // Already prepared...
        | None ->
            match ket.Expression with
            | Expression.Literal width -> prepare_literal ctx width
            | Expression.Constant value -> prepare_constant ctx value
            | Expression.Map (op, args) -> prepare_map ctx (op, args)
            | Expression.Where (target, op, args) -> prepare_where ctx (target, op, args)
            ==> fun (ctx', register) -> { ctx' with allocations = ctx'.allocations.Add(ket.Id, register) } |> Ok

    and prepare_many ctx kets =
        let init_one previous next = previous ==> fun ctx' -> prepare ctx' next
        kets |> List.fold init_one (Ok ctx)

    and prepare_literal ctx size =
        aleph.qsharp.ket.All.Run(sim, size |> int64, ctx.state).Result |> qsharp_result ctx

    and prepare_constant ctx value =
        let value = value |> toQValue
        aleph.qsharp.ket.Constant.Run(sim, value, ctx.state).Result |> qsharp_result ctx

    and prepare_where ctx (target, op, args) =
        prepare_map ctx (op, target :: args)
        ==> fun (ctx, f) ->
            let u = aleph.qsharp.ket.Filter.Run(sim, f, ctx.state).Result
            ({ ctx with state = u }, ctx.allocations.[target.Id]) |> Ok

    and prepare_map ctx (op, args) =
        prepare_many ctx args
        ==> fun ctx' ->
                match op with
                | Operator.Id -> (ctx', ctx'.allocations.[args.[0].Id]) |> Ok
                | Operator.Not -> map_unary ctx' (args.[0], aleph.qsharp.ket.Not.Run)
                | Operator.In values -> map_in ctx' (args.[0], values)
                | Operator.And -> map_binary ctx' (args.[0], args.[1], aleph.qsharp.ket.And.Run)
                | Operator.Or -> map_binary ctx' (args.[0], args.[1], aleph.qsharp.ket.Or.Run)
                | Operator.LessThanEquals -> map_binary ctx' (args.[0], args.[1], aleph.qsharp.ket.LessThanEqual.Run)
                | Operator.GreaterThan -> map_binary ctx' (args.[0], args.[1], aleph.qsharp.ket.GreaterThan.Run)
                | Operator.Equals -> map_binary ctx' (args.[0], args.[1], aleph.qsharp.ket.Equals.Run)
                | Operator.Add w -> map_binary ctx' (args.[0], args.[1], op_width w aleph.qsharp.ket.Add.Run)
                | Operator.Multiply w -> map_binary ctx' (args.[0], args.[1], op_width w aleph.qsharp.ket.Multiply.Run)
                | Operator.If-> map_if ctx' (args.[0], args.[1], args.[2])

    and op_width w lambda (sim, l, r, state) =
        lambda(sim, l, r, w, state)

    and map_unary ctx (ket, lambda) =
        let k = ctx.allocations.[ket.Id]
        lambda(sim, k, ctx.state).Result |> qsharp_result ctx

    and map_binary ctx (left, right, lambda) =
        let l = ctx.allocations.[left.Id]
        let r = ctx.allocations.[right.Id]
        lambda(sim, l, r, ctx.state).Result |> qsharp_result ctx

    and map_in ctx (ket, values) =
        let l = ctx.allocations.[ket.Id]
        let values = values |> List.map toQValue |> QArray
        let register = ctx.allocations.[ket.Id]
        aleph.qsharp.ket.InSet.Run(sim, values, register, ctx.state).Result |> qsharp_result ctx

    and map_if ctx (cond, onTrue, onFalse) =
        let c = ctx.allocations.[cond.Id]
        let t = ctx.allocations.[onTrue.Id]
        let f = ctx.allocations.[onFalse.Id]
        aleph.qsharp.ket.If.Run(sim, c, t, f, ctx.state).Result |> qsharp_result ctx

    and qsharp_result ctx value =
        let struct (u, r) = value
        let r = r |> Seq.head
        ({ ctx with state = u }, r) |> Ok

    interface QPU with
        (*
            Measure works by sampling the universe:
        *)
        member this.Measure(universe: IUniverse, kets: Ket list) =
            let u = universe :?> Universe
            u.Sample(kets) |> Ok

        (*
            Prepares a Quantum Universe from the given universe expression
         *)
        member this.Prepare(kets: Ket list) =
            let ctx =
                { allocations = Map.empty
                  state = universe.BigBang.Run(sim).Result }

            prepare_many ctx kets
            ==> fun ctx' -> Universe(sim, ctx'.state, ctx'.allocations) :> IUniverse |> Ok


module context =
    open Microsoft.Quantum.Simulation.Simulators

    let simulator = new SparseSimulator()

    let sample (kets: Ket list) =
        let ctx = { qpu = Processor(simulator)}
        sample ctx kets

    let sample_when (kets: Ket list, filter: Ket) =
        let ctx = { qpu = Processor(simulator)}
        sample_when ctx (kets, filter)