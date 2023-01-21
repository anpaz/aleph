namespace aleph.runtime.qpu.qsharp

open Microsoft.Quantum.Simulation.Core
open aleph.qsharp

open aleph.runtime.Eval

type QuantumState = universe.Universe
type QuantumValue = value.Value

type QuantumRegister =
    | One of register.Register
    | Many of register.Register list

type RegistersMap = Map<KetId, QuantumRegister>

module Convert =
    let toQValue =
        function
        | Bool b ->
            new QuantumValue(
                (if b then
                     (1, BOOL_REGISTER_SIZE)
                 else
                     (0, BOOL_REGISTER_SIZE))
            )
        | Int i -> new QuantumValue((i, INT_REGISTER_DEFAULT_SIZE))
        | _ -> failwith "not an int/bool"

    let toQTuple =
        function
        | Bool b -> [| (Bool b |> toQValue) |] |> QArray<QuantumValue>
        | Int i -> [| (Int i |> toQValue) |] |> QArray<QuantumValue>
        | Tuple t -> t |> List.map toQValue |> List.toArray |> QArray<QuantumValue>
        | _ -> failwith "not a tuple"

    let toQSet =
        function
        | Set s -> s |> Set.toArray |> Array.map toQTuple |> QArray<IQArray<QuantumValue>>
        | _ -> failwith "not a set"

    let toValue (result: IQArray<QuantumValue>) =
        let one (v: QuantumValue) =
            if v.size = BOOL_REGISTER_SIZE then
                if v.value = 1 then Bool true else Bool false
            else
                Int(int v.value)

        if result.Length = 1 then
            one result.[0]
        else
            Tuple(result |> Seq.map one |> Seq.toList)

    let toQArray register =
        match register with
        | One n -> QArray<register.Register>(n)
        | Many m -> QArray<register.Register>(m)

type Universe(sim: IOperationFactory, state: QuantumState, register: QuantumRegister) =
    let mutable value = None

    interface IUniverse with
        member this.CompareTo(obj: obj) : int = failwith "Not Implemented"

    member this.Sample() =
        match value with
        | Some v -> v
        | None ->
            let sample =
                universe.Sample.Run(sim, state, register |> Convert.toQArray).Result
                |> Convert.toValue

            value <- Some sample
            sample

    override this.ToString() =
        universe.Print.Run(sim, state).Result |> ignore
        "[see above]"

type QsharpContext =
    { graph: QuantumGraph
      state: QuantumState
      allocations: RegistersMap }

type Processor(sim: IOperationFactory) =

    let rec prepare (ctx: QsharpContext) (k: KetId) =
        if k < 0 then
            ctx |> Ok
        else
            match ctx.allocations.TryFind k with
            | Some columns -> ctx |> Ok // Already prepared...
            | None ->
                match ctx.graph.[k] with
                | KetExpression.Literal size -> prepare_literal ctx size
                | KetExpression.Join ketIds -> prepare_join ctx ketIds
                | KetExpression.Project (ketId, idx) -> prepare_project ctx (ketId, idx)
                | KetExpression.Map (ketId, lambda) -> prepare_map ctx (ketId, lambda)
                | KetExpression.Filter (ketId, filterId) -> prepare_filter ctx (ketId, filterId)

                ==> fun (ctx', register) -> { ctx' with allocations = ctx'.allocations.Add(k, register) } |> Ok

    and prepare_literal ctx size =
        ket.All.Run(sim, size |> int64, ctx.state).Result |> qsharp_result ctx

    and prepare_join ctx (ketIds: KetId list) =
        // prepare all elements in the join so they are allocated in the state:
        let ctx'' =
            ketIds
            |> List.fold (fun ctx' ketId -> ctx' ==> fun (ctx) -> prepare ctx ketId) (ctx |> Ok)

        // now, map the ketids to their corresponding column:
        ctx''
        ==> fun ctx'' ->
                let columns =
                    ketIds
                    |> List.map (fun ketId -> (ctx''.allocations.[ketId]))
                    |> List.fold
                        (fun idx ->
                            function
                            | QuantumRegister.One c -> idx @ [ c ]
                            | QuantumRegister.Many many -> idx @ many)
                        []

                (ctx'', QuantumRegister.Many columns) |> Ok

    and prepare_project ctx (ketId, index) =
        prepare ctx ketId
        ==> fun ctx' ->
                let columns = ctx'.allocations.[ketId]

                match columns with
                | QuantumRegister.Many columns -> (ctx', QuantumRegister.One columns.[index % columns.Length]) |> Ok
                | _ -> $"Invalid ket to project: {ketId}" |> Error

    and prepare_map ctx (ketId, lambda) =
        prepare ctx ketId
        ==> fun ctx' ->
                match lambda with
                | KetMapOperator.Not -> map_unary ctx' (ketId, ket.Not.Run)
                | KetMapOperator.Add -> map_binary ctx' (ketId, ket.Add.Run)
                | KetMapOperator.Multiply -> map_binary ctx' (ketId, ket.Multiply.Run)
                | KetMapOperator.Eq -> map_binary ctx' (ketId, ket.Equals.Run)
                | KetMapOperator.LessThanEqual -> map_binary ctx' (ketId, ket.LessThanEqual.Run)
                | KetMapOperator.GreaterThan -> map_binary ctx' (ketId, ket.GreaterThan.Run)
                | KetMapOperator.And -> map_binary ctx' (ketId, ket.And.Run)
                | KetMapOperator.Or -> map_binary ctx' (ketId, ket.Or.Run)
                | KetMapOperator.In s -> map_in ctx' (ketId, s)
                | KetMapOperator.Constant v -> map_constant ctx' v
                | KetMapOperator.If -> map_if ctx' ketId

    and map_constant ctx value =
        let value = value |> Convert.toQValue
        ket.Constant.Run(sim, value, ctx.state).Result |> qsharp_result ctx

    and map_unary ctx (ketId, lambda) =
        match ctx.allocations.[ketId] with
        | QuantumRegister.One l -> lambda(sim, l, ctx.state).Result |> qsharp_result ctx
        | err -> $"Invalid ket for unary operation: {ketId} points to columns {err}." |> Error

    and map_binary ctx (ketId, lambda) =
        match ctx.allocations.[ketId] with
        | QuantumRegister.Many [ l; r ] -> lambda(sim, l, r, ctx.state).Result |> qsharp_result ctx
        | err -> $"Invalid ket for binary expression: {ketId} points to columns {err}." |> Error

    and map_in ctx (ketId, v) =
        let values = v |> Convert.toQSet
        let registers = ctx.allocations.[ketId] |> Convert.toQArray
        ket.InSet.Run(sim, values, registers, ctx.state).Result |> qsharp_result ctx

    and map_if ctx ketId =
        match ctx.allocations.[ketId] with
        | QuantumRegister.Many [ c; t; e ] -> ket.If.Run(sim, c, t, e, ctx.state).Result |> qsharp_result ctx
        | err -> $"Invalid ket for if expression: {ketId} points to columns {err}." |> Error

    and prepare_filter ctx (ketId, filterId) =
        prepare ctx ketId
        ==> fun ctx ->
                prepare ctx filterId
                ==> fun ctx ->
                        match ctx.allocations.[filterId] with
                        | QuantumRegister.One f ->
                            let u = ket.Filter.Run(sim, f, ctx.state).Result
                            ({ ctx with state = u }, ctx.allocations.[ketId]) |> Ok
                        | err -> $"Invalid filter ketId. Pointing to {err}" |> Error

    and qsharp_result ctx value =
        let struct (u, r) = value

        let r =
            if r.Length = 1 then
                QuantumRegister.One r.[0]
            else
                QuantumRegister.Many(r.ToArray() |> Array.toList)

        ({ ctx with state = u }, r) |> Ok

    interface QPU with
        (*
            Measure works by sampling the universe:
        *)
        member this.Measure(universe: IUniverse) =
            let u = universe :?> Universe
            u.Sample() |> Ok

        (*
            Prepares a Quantum Universe from the given universe expression
         *)
        member this.Prepare(ketId: KetId, graph: QuantumGraph) =
            let ctx =
                { allocations = Map.empty
                  state = universe.BigBang.Run(sim).Result
                  graph = graph }

            prepare ctx ketId
            ==> fun ctx' -> Universe(sim, ctx'.state, ctx'.allocations.[ketId]) :> IUniverse |> Ok
