namespace aleph.runtime.qpu.qsharp

open Microsoft.Quantum.Simulation.Core

open aleph.qsharp
type QUniverse = Universe
type QValue = Value
type QRegister = Register
type QRegisters = IQArray<QRegister>

open aleph.parser.ast.typed
open aleph.runtime.Eval


module Convert =
    let BOOL_REGISTER_SIZE = 1
    let INT_REGISTER_DEFAULT_SIZE = 3

    let toQValue = function
        | Bool b -> new QValue((if b then (1,BOOL_REGISTER_SIZE) else (0,BOOL_REGISTER_SIZE)))
        | Int i -> new QValue((i, INT_REGISTER_DEFAULT_SIZE))
        | _ -> failwith "not an int/bool"
        
    let toQTuple = function
        | Bool b -> [| (Bool b |> toQValue) |] |> QArray<QValue>
        | Int i -> [| (Int i |> toQValue) |] |> QArray<QValue>
        | Tuple t -> 
            t 
            |> List.map toQValue 
            |> List.toArray
            |> QArray<QValue>
        | _ -> failwith "not a tuple"

    let toQSet = function
        | Set s -> 
            s 
            |> Set.toArray
            |> Array.map toQTuple
            |> QArray<IQArray<QValue>>
        | _ -> failwith "not a set"
        
    let toValue (result: IQArray<QValue>) = 
        let one (v: QValue) =
            if v.size = BOOL_REGISTER_SIZE then
                if v.value = 1 then Bool true else Bool false
            else
                Int (int v.value)
        if result.Length = 1 then
            one result.[0]
        else
            Tuple (result |> Seq.map one |> Seq.toList)

type QsharpContext = {
    allocations: Map<int, QRegisters>
    universe: QUniverse
    evalCtx: EvalContext
}

type Universe(sim: IOperationFactory, state: QUniverse, registers: QRegisters) =
    let mutable value = None

    interface IUniverse with
        member this.CompareTo(obj: obj): int = 
            failwith "Not Implemented"

    member this.Sample() =
        match value with
        | Some v ->
            v
        | None ->
            let sample = Sample.Run(sim, state, registers).Result |> Convert.toValue
            value <- Some sample
            sample

type Processor(sim: IOperationFactory) =

    let rec prepare_ket (ket : Ket, ctx: QsharpContext) =
        match ctx.allocations.TryFind ket.Id with
        | Some registers -> 
            (registers, ctx) |> Ok
        | None ->
            // need to prepare using the heap when the ket was created:
            let ctx' = { ctx with evalCtx = { ctx.evalCtx with heap = ket.Heap } }
            prepare (ket.StatePrep, ctx')
            ==> fun (registers, ctx') ->
                // Assign to the ket the columns returned by the preparation:
                // return the original heap
                let ctx = { 
                    ctx' with 
                        allocations = ctx'.allocations.Add (ket.Id, registers) 
                        evalCtx = ctx.evalCtx  }
                (registers, ctx) |> Ok

    and prepare (q, ctx) =
        match q with
        | Q.Var id -> prepare_var(id, ctx)
        | Q.Literal values -> prepare_literal (values, ctx)
        | Q.KetAll size -> prepare_ketall (size, ctx)

        | Q.Equals (left, right) -> prepare_equals (left, right, ctx)

        | Q.Not q -> prepare_not (q, ctx)
        | Q.And (left,right) -> prepare_and (left, right, ctx)
        | Q.Or (left,right) -> prepare_or (left, right, ctx)

        | Q.Project (q, index) -> prepare_project (q, index, ctx)
        | Q.Index (q, index) -> prepare_index (q, index, ctx)
        | Q.Join (left, right) -> prepare_join(left, right, ctx)        
        | Q.Block (stmts, value) -> prepare_block (stmts, value, ctx)

        | Q.CallMethod (method, args) ->  prepare_callmethod(method, args, ctx)

        | Q.Add _
        | Q.Multiply _
        | Q.Solve  _
        | Q.Block  _
        | Q.IfQuantum  _
        | Q.IfClassic  _
        | Q.Summarize _ ->
            $"Not implemented: {q}" |> Error

    and prepare_var (id, ctx) =
        match ctx.evalCtx.heap.TryFind id with
        | Some (Value.Ket ket) ->
            prepare_ket (ket, ctx)
            ==> fun (registers, ctx) ->
                (registers, ctx) |> Ok
        | _ ->
            $"Invalid variable: {id}. Expecting ket." |> Error

    and prepare_literal (values, ctx) =
        eval_classic(values, ctx.evalCtx)
        ==> fun (values, evalCtx) ->
            match values with
            | Value.Set w when w.IsEmpty->
                (new QArray<Register>() :> QRegisters, { ctx with evalCtx = evalCtx }) |> Ok
            | Value.Set _ ->
                ket.Literal.Run(sim, values |> Convert.toQSet, ctx.universe).Result
                |> qsharp_result { ctx with evalCtx = evalCtx }
            | _ -> 
                $"Invalid classic value for a ket literal: {values}" |> Error

    and prepare_ketall (size, ctx) =
        eval_classic (size, ctx.evalCtx)
        ==> fun (size, evalCtx) ->
            match size with
            | Value.Int i ->
                ket.All.Run(sim, i |> int64, ctx.universe).Result
                |> qsharp_result { ctx with evalCtx = evalCtx }
            | _ -> 
                $"Invalid ket_all size, expected int got: {size}" |> Error

    and prepare_project (q, index, ctx) =
        prepare (q, ctx)
        ==> fun (registers, ctx) ->
            let i = index |> int64
            (registers.Slice(new QRange(i, i)), ctx) |> Ok

    and prepare_index (q, index, ctx) =
        prepare (q, ctx)
        ==> fun (registers, ctx) ->
            eval_classic (index, ctx.evalCtx)
            ==> fun (index, evalCtx) ->
                match index with
                | Value.Int i ->
                    let ctx = { ctx with evalCtx = evalCtx }
                    let idx = i % registers.Count |> int64
                    (registers.Slice(new QRange(idx, idx)), ctx) |> Ok
                | _ -> $"Invalid index, expecting int value, got {index}" |> Error

    and prepare_join (left, right, ctx) =
        prepare (left, ctx)
        ==> fun (left, ctx) ->
            prepare (right, ctx)
            ==> fun (right, ctx) ->
                (QArray.Add(left, right) :> QRegisters, ctx) |> Ok

    and prepare_not (q, ctx) =
        prepare (q, ctx)
        ==> fun (source, ctx) ->
            match source.Length with
            | 1L ->
                ket.Not.Run(sim, source.[0], ctx.universe).Result
                |> qsharp_result ctx
            | _ -> 
                $"Invalid inputs for ket not. Expected one length registers, got:: {source.Length}" |> Error

    and prepare_and (left, right, ctx) =
        prepare (left, ctx)
        ==> fun (left, ctx) ->
            prepare (right, ctx)
            ==> fun (right, ctx) ->
                match (left.Length, right.Length) with
                | (1L, 1L) ->
                    ket.And.Run(sim, left.[0], right.[0], ctx.universe).Result
                    |> qsharp_result ctx
                | _ -> 
                    $"Invalid inputs for ket And. Expected one length registers, got: left:{left.Length} && right:{right.Length}" |> Error


    and prepare_or (left, right, ctx) =
        prepare (left, ctx)
        ==> fun (left, ctx) ->
            prepare (right, ctx)
            ==> fun (right, ctx) ->
                match (left.Length, right.Length) with
                | (1L, 1L) ->
                    ket.Or.Run(sim, left.[0], right.[0], ctx.universe).Result
                    |> qsharp_result ctx
                | _ -> 
                    $"Invalid inputs for ket And. Expected one length registers, got: left:{left.Length} && right:{right.Length}" |> Error

    and prepare_equals (left, right, ctx) =
        prepare (left, ctx)
        ==> fun (left, ctx) ->
            prepare (right, ctx)
            ==> fun (right, ctx) ->
                match (left.Length, right.Length) with
                | (1L, 1L) ->
                    ket.Equals.Run(sim, left.[0], right.[0], ctx.universe).Result
                    |> qsharp_result ctx
                | _ -> 
                    $"Invalid inputs for ket And. Expected one length registers, got: left:{left.Length} && right:{right.Length}" |> Error
                    
    and prepare_block (stmts, value, ctx) =
        eval_stmts (stmts, ctx.evalCtx)
        ==> fun evalCtx ->
            let ctx = { ctx with evalCtx = evalCtx }
            prepare (value, ctx)

    and prepare_callmethod (method, args, ctx) =
        prepare_method (method, args, ctx.evalCtx)
        ==> fun (body, argsCtx) ->
            eval (body, argsCtx)
            ==> fun (value, argsCtx) ->
                match value with
                | Value.Ket k -> 
                    let ctx' = { ctx with evalCtx = argsCtx }
                    prepare_ket (k, ctx')
                    ==> fun (value, ctx') ->
                        // return the heap back to the original state
                        let ctx = { ctx' with evalCtx = ctx.evalCtx }
                        (value, ctx) |> Ok
                | _ ->
                    $"Expecting a Ket result, got {value}" |> Error

    and qsharp_result ctx value =
        let struct (u, r) = value
        (r, { ctx with universe = u }) |> Ok


    interface QPU with
    
        member this.Measure (universe: IUniverse) =
            let u = universe :?> Universe
            u.Sample() |> Ok
        
        member this.Prepare (u, evalCtx) =
            assert (evalCtx.qpu = this)
            match u with
            | U.Prepare q ->
                eval_quantum (q, evalCtx)
                ==> fun (ket, evalCtx) ->
                    match ket with
                    | Value.Ket ket ->
                        let ctx = {
                            allocations = Map.empty
                            universe = BigBang.Run(sim).Result
                            evalCtx = evalCtx }
                        prepare_ket (ket, ctx)
                        ==> fun (registers, ctx) ->
                            (Value.Universe (Universe(sim, ctx.universe, registers)), ctx.evalCtx) |> Ok
                    | _ -> "" |> Error
            | U.Var id ->
                match evalCtx.heap.TryFind id with
                | Some (Value.Universe u) ->
                    (Value.Universe u, evalCtx) |> Ok
                | _ ->
                    $"Invalid variable: {id}. Expecting universe." |> Error
            | U.Block (stmts, body) ->
                eval_stmts (stmts, evalCtx)
                ==> fun evalCtx ->
                    (this :> QPU).Prepare (body, evalCtx)
            | U.CallMethod (method, args) ->
                eval_callmethod(method, args, evalCtx)
