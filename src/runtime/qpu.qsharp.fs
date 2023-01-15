namespace aleph.runtime.qpu.qsharp

open Microsoft.Quantum.Simulation.Core
open aleph.qsharp

type QUniverse = universe.Universe
type QValue = value.Value

type QRegister =
    | One of register.Register
    | Many of register.Register list

open aleph.parser.ast.typed
open aleph.runtime.Eval

module Convert =
    let toQValue =
        function
        | Bool b ->
            new QValue(
                (if b then
                     (1, BOOL_REGISTER_SIZE)
                 else
                     (0, BOOL_REGISTER_SIZE))
            )
        | Int i -> new QValue((i, INT_REGISTER_DEFAULT_SIZE))
        | _ -> failwith "not an int/bool"

    let toQTuple =
        function
        | Bool b -> [| (Bool b |> toQValue) |] |> QArray<QValue>
        | Int i -> [| (Int i |> toQValue) |] |> QArray<QValue>
        | Tuple t -> t |> List.map toQValue |> List.toArray |> QArray<QValue>
        | _ -> failwith "not a tuple"

    let toQSet =
        function
        | Set s -> s |> Set.toArray |> Array.map toQTuple |> QArray<IQArray<QValue>>
        | _ -> failwith "not a set"

    let toValue (result: IQArray<QValue>) =
        let one (v: QValue) =
            if v.size = BOOL_REGISTER_SIZE then
                if v.value = 1 then Bool true else Bool false
            else
                Int(int v.value)

        if result.Length = 1 then
            one result.[0]
        else
            Tuple(result |> Seq.map one |> Seq.toList)

    let toQArray register =
        match register  with
        | One n -> QArray<register.Register>(n)
        | Many m -> QArray<register.Register>(m)

type QsharpContext =
    { allocations: Map<int, QRegister>
      universe: QUniverse
      graph: QuantumGraph }

type Universe(sim: IOperationFactory, state: QUniverse, registers: QRegister) =
    let mutable value = None

    interface IUniverse with
        member this.CompareTo(obj: obj) : int = failwith "Not Implemented"

    member this.Sample(maxTries: int64) =
        match value with
        | Some v -> v
        | None ->
            let sample = universe.Sample.Run(sim, state, registers |> Convert.toQArray, maxTries).Result |> Convert.toValue
            value <- Some sample
            sample

    override this.ToString() =
        universe.Print.Run(sim, state).Result |> ignore
        "[see above]"

type Processor(sim: IOperationFactory, maxTries: int64) =

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
                | KetExpression.Project(ketId, idx) -> prepare_project ctx (ketId, idx)
                | KetExpression.Map(ketId, lambda) -> prepare_map ctx (ketId, lambda)
                | KetExpression.Filter(ketId, filterId, hint) -> prepare_filter ctx (ketId, filterId, hint)

                ==> fun (ctx', register) -> { ctx' with allocations = ctx'.allocations.Add(k, register) } |> Ok

    and prepare_literal ctx size =
        if size = 0 then 
            "All literals must have a size." |> Error
        else
            ket.All.Run(sim, size |> int64, ctx.universe).Result
            |> qsharp_result ctx

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
                            | QRegister.One c -> idx @ [ c ]
                            | QRegister.Many many -> idx @ many)
                        []

                (ctx'', QRegister.Many columns) |> Ok

    and prepare_project ctx (ketId, index) =
        prepare ctx ketId
        ==> fun ctx' ->
                let columns = ctx'.allocations.[ketId]

                match columns with
                | QRegister.Many columns -> (ctx', QRegister.One columns.[index % columns.Length]) |> Ok
                | _ -> $"Invalid ket to project: {ketId}" |> Error

    and prepare_map ctx (ketId, lambda) =
        prepare ctx ketId
        ==> fun ctx' ->
                match lambda with
                | KetMapOperator.Not -> map_unary ctx' (ketId, ket.Not.Run)
                | KetMapOperator.Add -> map_binary ctx' (ketId, ket.Add.Run)
                | KetMapOperator.Multiply -> map_binary ctx' (ketId, ket.Multiply.Run)
                | KetMapOperator.Equals -> map_binary ctx' (ketId, ket.Equals.Run)
                //| KetMapOperator.LessThan -> map_binary ctx' (ketId, ket.LessThan.Run)
                | KetMapOperator.And -> map_binary ctx' (ketId, ket.And.Run)
                | KetMapOperator.Or -> map_binary ctx' (ketId, ket.Or.Run)
                | KetMapOperator.In s -> map_in ctx' (ketId, s)
                | KetMapOperator.Constant v -> map_constant ctx' v
                | KetMapOperator.If -> map_if ctx' ketId
                | err -> $"Map not implemented: {err}" |> Error

    and map_constant ctx value =
        let value = value |> Convert.toQValue
        ket.Constant.Run(sim, value, ctx.universe).Result
        |> qsharp_result ctx

    and map_unary ctx (ketId, lambda) =
        match ctx.allocations.[ketId] with
        | QRegister.One l ->
            lambda(sim, l, ctx.universe).Result
            |> qsharp_result ctx
        | err -> $"Invalid ket for unary operation: {ketId} points to columns {err}." |> Error

    and map_binary ctx (ketId, lambda) =
        match ctx.allocations.[ketId] with
        | QRegister.Many [ l; r ] ->
            lambda(sim, l, r, ctx.universe).Result
            |> qsharp_result ctx
        | err ->
            $"Invalid ket for binary expression: {ketId} points to columns {err}."
            |> Error

    and map_in ctx (ketId, v) =
        let values = v |> Convert.toQSet
        let registers = ctx.allocations.[ketId] |> Convert.toQArray
        ket.InSet.Run(sim, values, registers, ctx.universe).Result
        |> qsharp_result ctx

    and map_if ctx ketId =
        match ctx.allocations.[ketId] with
        | QRegister.Many [ c; t; e ] ->
            ket.If.Run(sim, c, t, e, ctx.universe).Result
            |> qsharp_result ctx
        | err ->
            $"Invalid ket for if expression: {ketId} points to columns {err}."
            |> Error

    and prepare_filter ctx (ketId, filterId, hint) =
        prepare ctx ketId
        ==> fun ctx ->
                prepare ctx filterId
                ==> fun ctx ->
                    match  ctx.allocations.[filterId] with
                    | QRegister.One f ->
                        let u = ket.Filter.Run(sim, f, hint, ctx.universe).Result
                        ({ ctx with universe = u }, ctx.allocations.[ketId]) |> Ok
                    | err -> $"Invalid filter ketId. Pointing to {err}" |> Error

    and qsharp_result ctx value =
        let struct (u, r) = value
        let r = if r.Length = 1 then QRegister.One r.[0] else QRegister.Many (r.ToArray() |> Array.toList)
        ({ ctx with universe = u }, r) |> Ok


    interface QPU with
        member this.Measure(universe: IUniverse, evalCtx: EvalContext) =
            let u = universe :?> Universe
            (u.Sample(maxTries), evalCtx.graph) |> Ok

        member this.Prepare(u, evalCtx) =
            assert (evalCtx.qpu = this)

            match u with
            | U.Prepare q ->
                eval_quantum evalCtx q
                ==> fun (ket, graph) ->
                        match ket with
                        | Value.KetId ket ->
                            let ctx =
                                { allocations = Map.empty
                                  universe = universe.BigBang.Run(sim).Result
                                  graph = graph }

                            prepare ctx ket
                            ==> fun ctx -> (Value.Universe(Universe(sim, ctx.universe, ctx.allocations.[ket])), graph) |> Ok
                        | _ -> "" |> Error
            | U.Var id ->
                match evalCtx.heap.TryFind id with
                | Some(Value.Universe u) -> (Value.Universe u, evalCtx.graph) |> Ok
                | Some err -> $"Invalid variable: {err}. Expecting universe." |> Error
                | None -> $"Variable {id} not found in heap." |> Error
            | U.Block (stmts, body) ->
                eval_stmts evalCtx stmts
                ==> fun evalCtx -> (this :> QPU).Prepare(body, evalCtx)
            | U.CallMethod (method, args) -> eval_callmethod evalCtx (method, args)
