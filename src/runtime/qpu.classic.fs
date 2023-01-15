namespace aleph.runtime.qpu.classic

open aleph.parser.ast.typed
open aleph.runtime.Eval

(*

# QPU simulator

This class implements a classical simulator of a quantum processor.

The simulator works by representing the state of the quantum system as a table.
Each column of the table is a quantum register, each row represents a valid
combination of the tensor product across all registers. That is, each row
represents one of the possible outcomes that can be observed
when measuring the quantum state.

For example, preparing the state of two indepdent quantum registers
in which each can take values [0; 1] creates the table [0; 1] * [0; 1], i.e.:
[
    [ 0; 0 ]
    [ 0; 1 ]
    [ 1; 0 ]
    [ 1; 1 ]
]

The state is prepared from one or more quantum expressions (Q). Each expression
is assigned to a ket, which receives a unique id.
Each ket is prepared only once, as such the simulator memory keeps 
track of which kets have already been allocated by mapping the ket's id
to a list of columns.

For details about how each type of expression updates the state, see comments
of each prepare method below...

*)

type ColumnIndex =
    | One of int
    | Many of int list

type ColumnsMap = Map<KetId, ColumnIndex>

type QuantumContext =
    { graph: QuantumGraph
      state: Value list list
      allocations: ColumnsMap }

type Universe(state: Value list list, columns: ColumnIndex) =
    let random = System.Random()
    let mutable value = None

    interface IUniverse with
        member this.CompareTo(obj: obj) : int = failwith "Not Implemented"

    member val State = state
    member val Columns = columns

    (*
        Sample works by randomly picking a row from the universe with the same probability
        from the universe, and then projecting (selecting) only the columns
        associated with the ket.
        Once measured, the universe is collapsed to this value, and next time it is measured
        it will return the same value.
    *)
    member this.Sample() =
        match value with
        | Some v -> v
        | None ->
            let pick_world () =
                match state.Length with
                // Universe collapsed:
                | 1 -> state.[0]
                // Empty universe, collapse to random value
                | 0 ->
                    let c =
                        match columns with
                        | One c -> [ c ]
                        | Many c -> c

                    let row =
                        seq { for i in 0 .. (c |> List.max) -> (Value.Int(random.Next())) }
                        |> Seq.toList

                    row
                // Select a random row, and collapse to this value:
                | n ->
                    let i = int (random.NextDouble() * (double (n)))
                    state.Item i

            let sample = pick_world () |> (Universe.project columns)
            value <- Some sample
            sample

    override this.ToString() =
        sprintf "%A" (seq { for i in state -> i |> (Universe.project columns) } |> Seq.toList)

    static member project columns (row: Value list) =
        match columns with
        | One c -> row.[c]
        | Many columns -> columns |> List.fold (fun result i -> result @ [ row.[i] ]) [] |> Tuple

type Processor() =

    (* 
        Preparing a Ket consists of first checking if the Ket is already allocated
        in memory, if so, it's a no-op
        otherwise, it prepares the state of the Ket's state prep expression
        and allocates in memory the columns returned by the state prep to this ket.
     *)
    let rec prepare (ctx: QuantumContext) (k: KetId) =
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
                | KetExpression.Filter(ketId, filterId, _) -> prepare_filter ctx (ketId, filterId)

                ==> fun (ctx', column) -> { ctx' with allocations = ctx'.allocations.Add(k, column) } |> Ok

    and prepare_literal ctx size =
        match size with
        | 0 -> "All literals must have a size." |> Error
        | 1 -> [ Value.Bool false; Value.Bool true ] |> Ok // Literal Kets of size 1, are always boolean values.
        | n -> seq { 0 .. (int (2.0 ** n)) - 1 } |> Seq.map (Value.Int) |> Seq.toList |> Ok
        ==> fun values ->
            let new_state = tensor_product ctx.state values
            let new_column = if new_state.IsEmpty then 0 else new_state.Head.Length - 1

            ({ ctx with state = new_state }, ColumnIndex.One new_column) |> Ok

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
                            | ColumnIndex.One c -> idx @ [ c ]
                            | ColumnIndex.Many many -> idx @ many)
                        []

                (ctx'', ColumnIndex.Many columns) |> Ok

    and prepare_project (ctx: QuantumContext) (ketId, index) =
        prepare ctx ketId
        ==> fun ctx' ->
                let columns = ctx'.allocations.[ketId]

                match columns with
                | ColumnIndex.Many columns -> (ctx', ColumnIndex.One columns.[index % columns.Length]) |> Ok
                | _ -> $"Invalid ket to project: {ketId}" |> Error

    and prepare_map ctx (ketId, lambda) =
        prepare ctx ketId
        ==> fun ctx' ->
                match lambda with
                | KetMapOperator.Not -> map_unary ctx' (ketId, Value.Not)
                | KetMapOperator.Add -> map_binary ctx' (ketId, Value.(+))
                | KetMapOperator.Multiply -> map_binary ctx' (ketId, Value.(*))
                | KetMapOperator.Equals -> map_binary ctx' (ketId, Value.(==))
                | KetMapOperator.LessThan -> map_binary ctx' (ketId, Value.LessThan)
                | KetMapOperator.And -> map_binary ctx' (ketId, Value.And)
                | KetMapOperator.Or -> map_binary ctx' (ketId, Value.Or)
                | KetMapOperator.In s -> map_in ctx' (ketId, s)
                | KetMapOperator.Constant v -> map_constant ctx' v
                | KetMapOperator.If -> map_if ctx' ketId

    and map_constant ctx value =
        match value with
        | Bool _
        | Int _ ->
            if ctx.state.IsEmpty then
                let new_column = 0 // last column
                let new_state = [ [ value ] ]
                ({ ctx with state = new_state }, ColumnIndex.One new_column) |> Ok
            else
                let new_column = ctx.state.Head.Length

                let new_state =
                    seq {
                        for row in ctx.state do
                            row @ [ value ]
                    }
                    |> Seq.toList

                let ctx = { ctx with state = new_state }
                ({ ctx with state = new_state }, ColumnIndex.One new_column) |> Ok
        | error -> $"Invalid value for a constant Ket: {value}." |> Error

    and map_unary ctx (ketId, lambda: Value -> Value) =
        match ctx.allocations.[ketId] with
        | ColumnIndex.One l ->
            let new_column = ctx.state.Head.Length

            let new_state =
                seq {
                    for row in ctx.state do
                        row @ [ lambda (row.[l]) ]
                }
                |> Seq.toList

            ({ ctx with state = new_state }, ColumnIndex.One new_column) |> Ok
        | error -> $"Invalid ket for unary operation: {ketId} points to columns {error}." |> Error

    and map_binary ctx (ketId, lambda: Value * Value -> Value) =
        match ctx.allocations.[ketId] with
        | ColumnIndex.Many [ l; r ] ->
            let new_column = ctx.state.Head.Length

            let new_state =
                seq {
                    for row in ctx.state do
                        row @ [ lambda (row.[l], row.[r]) ]
                }
                |> Seq.toList

            ({ ctx with state = new_state }, ColumnIndex.One new_column) |> Ok
        | error ->
            $"Invalid ket for binary expression: {ketId} points to columns {error}."
            |> Error

    and map_in ctx (ketId, v) =
        match v with
        | Value.Set s ->
            let columns = ctx.allocations.[ketId]
            let new_column = ctx.state.Head.Length

            let new_state =
                seq {
                    for row in ctx.state do
                        row @ [ Value.Bool(s.Contains(Universe.project columns row)) ]
                }
                |> Seq.toList

            ({ ctx with state = new_state }, ColumnIndex.One new_column) |> Ok
        | _ -> $"In map for preparation expects a set." |> Error

    and map_if ctx ketId =
        match ctx.allocations.[ketId] with
        | ColumnIndex.Many [ c; t; e ] ->
            let new_column = ctx.state.Head.Length

            let new_state =
                seq {
                    for row in ctx.state do
                        row @ [ if row.[c] = (Bool true) then row.[t] else row.[e] ]
                }
                |> Seq.toList

            ({ ctx with state = new_state }, ColumnIndex.One new_column) |> Ok
        | error -> $"Invalid ket for if expression: {ketId} points to columns {error}." |> Error

    and prepare_filter ctx (ketId, filterId) =
        prepare ctx ketId
        ==> fun ctx' ->
                prepare ctx' filterId
                ==> fun ctx'' ->
                        let column = ctx''.allocations.[filterId]

                        match column with
                        | ColumnIndex.One c ->
                            let new_state = List.filter (fun (r: Value list) -> r.[c] = (Bool true)) ctx''.state
                            ({ ctx'' with state = new_state }, ctx''.allocations.[ketId]) |> Ok
                        | _ -> $"Invalid filter index: {filterId}" |> Error

    and tensor_product left right : Value list list =
        let as_list =
            function
            | Value.Bool b -> [ Value.Bool b ]
            | Value.Int i -> [ Value.Int i ]
            | Value.Tuple t -> t
            | _ -> failwith "Invalid tuple value"

        if left.IsEmpty then
            seq { for j in right -> (j |> as_list) }
        else
            seq {
                for i in left do
                    for j in right -> i @ (j |> as_list)
            }
        |> Seq.toList

    (*
        Implements the QPU interface used by the classical eval to interact with quantum
        expressions.
     *)
    interface QPU with
        (*
            Measure works by sampling the universe:
        *)
        member this.Measure(universe: IUniverse, evalCtx: EvalContext) =
            let u = universe :?> Universe
            (u.Sample(), evalCtx.graph) |> Ok

        (*
            Prepares a Quantum Universe from the given universe expression
         *)
        member this.Prepare(u, evalCtx: EvalContext) =
            assert (evalCtx.qpu = this)

            match u with
            | U.Prepare q ->
                eval_quantum evalCtx q
                ==> fun (value, graph) ->
                        match value with
                        | Value.KetId ket ->
                            let ctx =
                                { allocations = Map.empty
                                  state = []
                                  graph = graph }

                            prepare ctx ket
                            ==> fun ctx -> (Value.Universe(Universe(ctx.state, ctx.allocations.[ket])), graph) |> Ok
                        | _ -> "" |> Error
            | U.Var id ->
                match evalCtx.heap.TryFind id with
                | Some(Value.Universe u) -> (Value.Universe u, evalCtx.graph) |> Ok
                | Some err -> $"Invalid variable: {err}. Expecting universe." |> Error
                | None -> $"Variable {id} not found in heap." |> Error
            | U.Block(stmts, body) -> eval_stmts evalCtx stmts ==> fun evalCtx -> (this :> QPU).Prepare(body, evalCtx)
            | U.CallMethod(method, args) -> eval_callmethod evalCtx (method, args)
