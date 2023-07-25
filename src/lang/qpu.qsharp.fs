namespace aleph.qpu.qsharp

open System

open aleph.utils
open aleph.kets

open Microsoft.Quantum.Simulation.Core
open aleph.qsharp

type UniverseInfo = universe.UniverseInfo
type QuantumValue = value.Value
type QuantumRegister = register.Register

type RegistersMap = Map<KetId, QuantumRegister>

type QsharpContext =
    { width: int64
      allocations: RegistersMap
      operators: ket.Operator list
      oracles: ket.Oracle list }

type Universe(sim: IOperationFactory, ctx: QsharpContext) =
    let mutable value = None

    let info =
        UniverseInfo(
            (ctx.width,
             ctx.allocations.Values |> Seq.distinct |> QArray :> IQArray<register.Register>,
             ctx.operators |> QArray :> IQArray<ket.Operator>,
             ctx.oracles |> QArray :> IQArray<ket.Oracle>)
        )

    let sample (registers: QuantumRegister list) =
        universe.Sample.Run(sim, info, registers |> QArray).Result
        |> Seq.map (fun i -> i.value |> int)
        |> Seq.toList

    interface IUniverse with
        member this.Sample(kets: KetValue list) =
            let registers = kets |> List.map (fun k -> ctx.allocations.[k.Id])

            match value with
            | Some v -> v |> Ok
            | None ->
                value <- sample registers |> Some
                value.Value |> Ok

        member this.Histogram(kets: KetValue list, rounds: int) =
            let registers = kets |> List.map (fun k -> ctx.allocations.[k.Id])

            let add_sample (map: Map<int list, int>) _ =
                let value = sample registers

                if map.ContainsKey(value) then
                    map.Add(value, map.[value] + 1)
                else
                    map.Add(value, 1)

            seq { 1..rounds } |> Seq.fold add_sample Map.empty |> Ok

    override this.ToString() =
        universe.Print.Run(sim, info).Result |> ignore
        "[see above]"

type Processor(sim: IOperationFactory) =

    let toQValue (i: int) =
        let w = int_width i |> int64
        new QuantumValue((i |> int64, w))

    let rec init ctx (ket: KetValue) =
        match ctx.allocations.TryFind ket.Id with
        | Some _ -> ctx // Already prepared...
        | None ->
            match ket.Expression with
            | KetExpression.Literal width -> init_literal ctx ket.Id width
            | KetExpression.Constant value -> init_constant ctx ket.Id value
            | KetExpression.Map(op, args) -> init_map ctx ket.Id (op, args)
            | KetExpression.Where(target, op, args) -> init_where ctx ket.Id (target, op, args)

    and init_many ctx kets = kets |> List.fold init ctx

    and init_literal ctx ketid size =
        let register =
            aleph.qsharp.register.NewLiteral.Run(sim, ctx.width, size |> int64).Result

        { ctx with
            width = ctx.width + (size |> int64)
            allocations = ctx.allocations.Add(ketid, register) }

    and init_constant ctx ketid value =
        let size = int_width value |> int64
        let value = value |> toQValue
        let register = aleph.qsharp.register.NewOutput.Run(sim, ctx.width, size).Result
        let op = aleph.qsharp.ket.Constant.Run(sim, value, register).Result

        { ctx with
            width = ctx.width + size
            allocations = ctx.allocations.Add(ketid, register)
            operators = ctx.operators @ [ op ] }

    and init_where ctx ketid (target, op, args) =
        let ctx = init_map ctx ketid (op, target :: args)
        let register = ctx.allocations.[ketid]
        let target = ctx.allocations[target.Id]
        let oracle = aleph.qsharp.ket.Filter.Run(sim, register).Result

        { ctx with
            allocations = ctx.allocations.Add(ketid, target)
            oracles = oracle :: ctx.oracles }

    and init_map ctx ketid (op, args) =
        let ctx = init_many ctx args

        match op with
        | Operator.Id -> map_id ctx ketid args.[0]
        | Operator.Not -> map_unary ctx ketid (args.[0], aleph.qsharp.ket.Not.Run)
        | Operator.In values -> map_in ctx ketid (args.[0], values)
        | Operator.And -> map_binary ctx ketid (args.[0], args.[1], 1, aleph.qsharp.ket.And.Run)
        | Operator.Or -> map_binary ctx ketid (args.[0], args.[1], 1, aleph.qsharp.ket.Or.Run)
        | Operator.LessThanEquals -> map_binary ctx ketid (args.[0], args.[1], 1, aleph.qsharp.ket.LessThanEqual.Run)
        | Operator.GreaterThan -> map_binary ctx ketid (args.[0], args.[1], 1, aleph.qsharp.ket.GreaterThan.Run)
        | Operator.Eq -> map_binary ctx ketid (args.[0], args.[1], 1, aleph.qsharp.ket.Equals.Run)
        | Operator.Add w -> map_binary ctx ketid (args.[0], args.[1], w, aleph.qsharp.ket.Add.Run)
        | Operator.Multiply w -> map_binary ctx ketid (args.[0], args.[1], w, aleph.qsharp.ket.Multiply.Run)
        | Operator.If -> map_if ctx ketid (args.[0], args.[1], args.[2])

    and map_id ctx ketid src = 
        { ctx with
            allocations = ctx.allocations.Add(ketid, ctx.allocations.[src.Id]) }

    and map_unary ctx ketid (ket, lambda) =
        let k = ctx.allocations.[ket.Id]
        let target = aleph.qsharp.register.NewOutput.Run(sim, ctx.width, 1).Result
        let op = lambda(sim, k, target).Result

        { ctx with
            width = ctx.width + (1 |> int64)
            allocations = ctx.allocations.Add(ketid, target)
            operators = ctx.operators @ [ op ] }

    and map_binary ctx ketid (left, right, size, lambda) =
        let l = ctx.allocations.[left.Id]
        let r = ctx.allocations.[right.Id]
        let target = aleph.qsharp.register.NewOutput.Run(sim, ctx.width, size).Result
        let op = lambda(sim, l, r, target).Result

        { ctx with
            width = ctx.width + (size |> int64)
            allocations = ctx.allocations.Add(ketid, target)
            operators = ctx.operators @ [ op ] }

    and map_in ctx ketid (ket, values) =
        let k = ctx.allocations.[ket.Id]
        let values = values |> List.map toQValue |> QArray
        let width = 1 |> int64
        let target = aleph.qsharp.register.NewOutput.Run(sim, ctx.width, width).Result
        let op = aleph.qsharp.ket.InSet.Run(sim, values, k, target).Result

        { ctx with
            width = ctx.width + width
            allocations = ctx.allocations.Add(ketid, target)
            operators = ctx.operators @ [ op ] }

    and map_if ctx ketid (cond, onTrue, onFalse) =
        let c = ctx.allocations.[cond.Id]
        let t = ctx.allocations.[onTrue.Id]
        let f = ctx.allocations.[onFalse.Id]
        let width = Math.Max(t.width, f.width)
        let target = aleph.qsharp.register.NewOutput.Run(sim, ctx.width, width).Result
        let op = aleph.qsharp.ket.If.Run(sim, c, t, f, target).Result

        { ctx with
            width = ctx.width + width
            allocations = ctx.allocations.Add(ketid, target)
            operators = ctx.operators @ [ op ] }

    interface QPU with
        member this.Prepare(kets: KetValue list) =
            let ctx =
                { width = 0
                  allocations = Map.empty
                  operators = []
                  oracles = [] }

            let ctx' = init_many ctx kets
            Universe(sim, ctx') :> IUniverse |> Ok


module context =
    open Microsoft.Quantum.Simulation.Simulators

    let simulator = new SparseSimulator()

    let prepare (kets: KetValue list) =
        let ctx = { qpu = Processor(simulator) }

        match prepare ctx kets with
        | Ok universe -> universe
        | Error err -> failwith err

    let sample (kets: KetValue list) =
        let ctx = { qpu = Processor(simulator) }

        match sample ctx kets with
        | Ok values -> values
        | Error err -> failwith err

    let sample_when (kets: KetValue list) (filter: KetValue) =
        let ctx = { qpu = Processor(simulator) }

        match sample_when ctx kets filter with
        | Ok values -> values
        | Error err -> failwith err

    let histogram (kets: KetValue list) (rounds: int) =
        let u = prepare kets

        match u.Histogram(kets, rounds) with
        | Ok values -> values
        | Error err -> failwith err
