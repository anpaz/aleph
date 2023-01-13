namespace aleph.runtime.qpu.classicv5

open aleph.parser.ast.typed
open aleph.runtime.EvalV5

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

    let project_columns (row: Value list) =
        match columns with
        | One c ->
            row.[c]
        | Many columns ->
            columns |> List.fold (fun result i -> result @ [ row.[i] ]) [] |> Tuple

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
                    let c = match columns with | One c -> [c] | Many c -> c
                    let row =
                        seq { for i in 0 .. (c |> List.max) -> (Value.Int(random.Next())) }
                        |> Seq.toList

                    row
                // Select a random row, and collapse to this value:
                | n ->
                    let i = int (random.NextDouble() * (double (n)))
                    state.Item i

            let sample = pick_world () |> project_columns
            value <- Some sample
            sample

    override this.ToString() =
        sprintf "%A" (seq { for i in state -> i |> project_columns } |> Seq.toList)

type Processor() =

    (* 
        Preparing a Ket consists of first checking if the Ket is already allocated
        in memory, if so, it's a no-op
        otherwise, it prepares the state of the Ket's state prep expression
        and allocates in memory the columns returned by the state prep to this ket.
     *)
    let rec prepare (ctx: QuantumContext) (k: KetId) =
        match ctx.allocations.TryFind k with
        | Some columns -> ctx |> Ok // Already prepared...
        | None ->
            match ctx.graph.[k] with
            | KetExpression.Literal size -> prepare_ketall ctx size
            | KetExpression.Join ketIds -> prepare_join ctx ketIds
            | KetExpression.Project (ket, idx) -> prepare_project ctx (ket, idx)
            | _ -> $"Prepare not implemented" |> Error

            |> process_result ctx k

    // (*
    //     Prepares the Universe for the given expression, and returns the index of the
    //     columns corresponding to the return value of the expression.
    //  *)
    // and prepare (q, ctx) =
    //     match q with
    //     | Q.Var id -> prepare_var (id, ctx)

    //     | Q.Constant value -> prepare_constant (value, ctx)
    //     | Q.Ket c -> prepare_literal (c, ctx)
    //     | KetAll size -> prepare_ketall (size, ctx)

    //     | Equals (left, right) -> prepare_equals (left, right, ctx)

    //     | Add (left, right) -> prepare_add (left, right, ctx)
    //     | Multiply (left, right) -> prepare_multiply (left, right, ctx)

    //     | Not q -> prepare_not (q, ctx)
    //     | And (left, right) -> prepare_and (left, right, ctx)
    //     | Or (left, right) -> prepare_or (left, right, ctx)

    //     | Project (q, index) -> prepare_project (q, index, ctx)
    //     | Index (q, index) -> prepare_index (q, index, ctx)
    //     | Join (left, right) -> prepare_join (left, right, ctx)

    //     | IfQuantum (condition, then_q, else_q) -> prepare_if_q (condition, then_q, else_q, ctx)
    //     | IfClassic (condition, then_q, else_q) -> prepare_if_c (condition, then_q, else_q, ctx)
    //     | Filter (ket, condition, hint) -> prepare_filter (ket, condition, hint, ctx)

    //     | Q.Block (stmts, value) -> prepare_block (stmts, value, ctx)
        
    //     | Q.CallMethod (method, args) -> prepare_callmethod (method, args, ctx)

    // (*
    //     Finds the var as a Ket in the heap, and then calls prepare on the corresponding ket.
    //     After calling prepare on the ket it will be allocated in memory, as such
    //     this method returns the columns associated with the ket accordingly.
    //  *)
    // and prepare_var (id, ctx) =
    //     eval_var (id, ctx.evalCtx)
    //     ==> fun (value, evalCtx) ->
    //             match value with
    //             | Value.Ket ket ->
    //                 prepare_ket (ket, { ctx with evalCtx = evalCtx })
    //                 ==> fun (columns, ctx) -> (columns, ctx) |> Ok
    //             | _ -> $"Unknown variable: {id}. Expecting ket." |> Error

    // (*
    //     Adding a new literal involves updating the quantum state 
    //     by doing the tensor product of the current state with the new values
    //     from the literal.
    //     It returns the columns allocated for the new values.
    //  *)
    // and prepare_literal (values, ctx) =
    //     eval_classic (values, ctx.evalCtx)
    //     ==> fun (values, evalCtx) ->
    //             match values with
    //             | Value.Set values ->
    //                 let old_size = if ctx.state.IsEmpty then 0 else ctx.state.Head.Length
    //                 let new_state = tensor_product ctx.state (values |> Set.toList)
    //                 let new_size = if new_state.IsEmpty then 0 else new_state.Head.Length
    //                 let new_columns = [ old_size .. new_size - 1 ]

    //                 let ctx =
    //                     { ctx with
    //                         state = new_state
    //                         evalCtx = evalCtx }

    //                 (new_columns, ctx) |> Ok
    //             | _ -> $"Invalid classic value for a ket literal: {values}" |> Error

    and process_result ctx k value =
        value
        ==> fun(state, column) ->
            { ctx with state = state; allocations = ctx.allocations.Add (k, column) } |> Ok

    and prepare_ketall ctx size : Result<Value list list * ColumnIndex, string> =
        let values = seq { 0 .. (int (2.0 ** size)) - 1 } |> Seq.map (Value.Int) |> Seq.toList
        let new_state = tensor_product ctx.state values
        let new_column = if new_state.IsEmpty then 0 else new_state.Head.Length - 1

        (new_state, ColumnIndex.One new_column) |> Ok

    // (*
    //     Adds a new column to the state, whose value is 
    //     the same for all rows.
    //  *)
    // and prepare_constant (value, ctx) =
    //     eval_classic (value, ctx.evalCtx)
    //     ==> fun (value, evalCtx) ->
    //         if ctx.state.IsEmpty then
    //             let new_columns = [ 0 ] // last column
    //             let new_state = [ [ value ] ]
    //             let ctx = { ctx with state = new_state; evalCtx = evalCtx }
    //             (new_columns, ctx) |> Ok
    //         else
    //             let mem_size = ctx.state.Head.Length
    //             let new_columns = [ mem_size ] // last column

    //             let new_state =
    //                 seq {
    //                     for row in ctx.state do
    //                         row @ [ value ]
    //                 }
    //                 |> Seq.toList
    //             let ctx = { ctx with state = new_state; evalCtx = evalCtx }
    //             (new_columns, ctx) |> Ok

            

    // (*
    //     Adds a new column to the state, whose value is 
    //     the addition of the values in the columns from the corresponding input expressions.
    //     It returns the new column.
    //  *)
    // and prepare_add (left, right, ctx) =
    //     prepare (left, ctx)
    //     ==> fun (left, ctx) ->
    //             prepare (right, ctx)
    //             ==> fun (right, ctx) ->
    //                     match (left, right) with
    //                     | ([ l ], [ r ]) ->
    //                         let mem_size = ctx.state.Head.Length
    //                         let new_columns = [ mem_size ] // last column

    //                         let new_state =
    //                             seq {
    //                                 for row in ctx.state do
    //                                     row @ [ row.[l] + row.[r] ]
    //                             }
    //                             |> Seq.toList

    //                         let ctx = { ctx with state = new_state }
    //                         (new_columns, ctx) |> Ok
    //                     | _ -> $"Invalid inputs for ket addition: {left} + {right}" |> Error

    // (*
    //     Adds a new column to the state, whose value is 
    //     the multiplication of the values in the columns from the corresponding input expressions.
    //     It returns the new column.
    //  *)
    // and prepare_multiply (left, right, ctx) =
    //     prepare (left, ctx)
    //     ==> fun (left, ctx) ->
    //             prepare (right, ctx)
    //             ==> fun (right, ctx) ->
    //                     match (left, right) with
    //                     | ([ l ], [ r ]) ->
    //                         let mem_size = ctx.state.Head.Length
    //                         let new_columns = [ mem_size ] // last column

    //                         let new_state =
    //                             seq {
    //                                 for row in ctx.state do
    //                                     row @ [ row.[l] * row.[r] ]
    //                             }
    //                             |> Seq.toList

    //                         let ctx = { ctx with state = new_state }
    //                         (new_columns, ctx) |> Ok
    //                     | _ -> $"Invalid inputs for ket addition: {left} + {right}" |> Error

    (*
        Returns the concatenation of the results from the input expressions.
     *)
    and prepare_join ctx (ketIds: KetId list) =
        // prepare all elements in the join so they are allocated in the state:
        let ctx'' =
            ketIds
            |> List.fold (fun ctx' ketId -> ctx' ==> fun (ctx) -> prepare ctx ketId) (ctx |> Ok)

        // now, map the ketids to their corresponding column:
        ctx'' ==> fun ctx'' ->
            let columns =
                ketIds 
                |> List.map (fun ketId -> (ctx''.allocations.[ketId]))
                |> List.fold (fun idx -> function ColumnIndex.One c -> idx @ [c] | ColumnIndex.Many many -> idx @ many ) []

            (ctx''.state, ColumnIndex.Many columns) |> Ok


    // (*
    //     Returns the the column from the input expression corresponding to the given index.
    //  *)
    and prepare_project (ctx: QuantumContext) (ket, index) =
        match ctx.graph.[ket] with
        | KetExpression.Join ketids ->
            let k' = ketids.[index]
            prepare ctx k'
            ==> fun ctx' ->
                (ctx'.state,  ctx'.allocations.[k']) |> Ok
        | _ ->
            $"Invalid project expression. Project can only be applied to the result of Join expressions." |> Error


        

    // (*
    //     Adds a new column to the state, whose value is 
    //     the negation of the original column.
    //     It returns the new column.
    //  *)
    // and prepare_not (q, ctx) =
    //     prepare (q, ctx)
    //     ==> fun (columns, ctx) ->
    //             match columns with
    //             | [ v ] ->
    //                 let mem_size = ctx.state.Head.Length
    //                 let new_columns = [ mem_size ] // last column

    //                 let new_state =
    //                     seq {
    //                         for row in ctx.state do
    //                             row @ [ (Value.Not row.[v]) ]
    //                     }
    //                     |> Seq.toList

    //                 let ctx = { ctx with state = new_state }
    //                 (new_columns, ctx) |> Ok
    //             | _ -> $"Invalid inputs for ket not: {q}" |> Error

    // (*
    //     Adds a new column to the state, whose value is 
    //     true iff the values in the columns from the corresponding input expressions are both true.
    //     It returns the new column.
    //  *)
    // and prepare_and (left, right, ctx) =
    //     prepare (left, ctx)
    //     ==> fun (left, ctx) ->
    //             prepare (right, ctx)
    //             ==> fun (right, ctx) ->
    //                     match (left, right) with
    //                     | ([ l ], [ r ]) ->
    //                         let mem_size = ctx.state.Head.Length
    //                         let new_columns = [ mem_size ] // last column

    //                         let new_state =
    //                             seq {
    //                                 for row in ctx.state do
    //                                     row @ [ Value.And(row.[l], row.[r]) ]
    //                             }
    //                             |> Seq.toList

    //                         let ctx = { ctx with state = new_state }
    //                         (new_columns, ctx) |> Ok
    //                     | _ -> $"Invalid inputs for ket equals: {left} && {right}" |> Error

    // (*
    //     Adds a new column to the state, whose value is 
    //     true iff the values in the columns from the corresponding input expressions are both true.
    //     It returns the new column.
    //  *)
    // and prepare_or (left, right, ctx) =
    //     prepare (left, ctx)
    //     ==> fun (left, ctx) ->
    //             prepare (right, ctx)
    //             ==> fun (right, ctx) ->
    //                     match (left, right) with
    //                     | ([ l ], [ r ]) ->
    //                         let mem_size = ctx.state.Head.Length
    //                         let new_columns = [ mem_size ] // last column

    //                         let new_state =
    //                             seq {
    //                                 for row in ctx.state do
    //                                     row @ [ Value.Or(row.[l], row.[r]) ]
    //                             }
    //                             |> Seq.toList

    //                         let ctx = { ctx with state = new_state }
    //                         (new_columns, ctx) |> Ok
    //                     | _ -> $"Invalid inputs for ket equals: {left} && {right}" |> Error

    // (*
    //     Adds a new column to the state, whose value is 
    //     true iff the values in the columns from the corresponding input expressions are equal.
    //     It returns the new column.
    //  *)
    // and prepare_equals (left, right, ctx) =
    //     prepare (left, ctx)
    //     ==> fun (left, ctx) ->
    //             prepare (right, ctx)
    //             ==> fun (right, ctx) ->
    //                     match (left, right) with
    //                     | ([ l ], [ r ]) ->
    //                         let mem_size = ctx.state.Head.Length
    //                         let new_columns = [ mem_size ] // last column

    //                         let new_state =
    //                             seq {
    //                                 for row in ctx.state do
    //                                     row @ [ row.[l] == row.[r] ]
    //                             }
    //                             |> Seq.toList

    //                         let ctx = { ctx with state = new_state }
    //                         (new_columns, ctx) |> Ok
    //                     | _ -> $"Invalid inputs for ket equals: {left} == {right}" |> Error

    // (*
    //     Filters the state to only those records that match the given condition.
    //     It returns the columns of the specified ket.
    //  *)
    // and prepare_filter (ket, condition, hint, ctx) =
    //     prepare (condition, ctx)
    //     ==> fun (cond, ctx) ->
    //             prepare (ket, ctx)
    //             ==> fun (ket, ctx) ->
    //                 eval_classic (hint, ctx.evalCtx)
    //                 ==> fun (_, _) ->
    //                     let new_state =
    //                         List.filter (fun (r: Value list) -> r.[cond.Head] = (Bool true)) ctx.state

    //                     let ctx = { ctx with state = new_state }
    //                     (ket, ctx) |> Ok

    // (*
    //     Prepares all: the condition, the then_q and the else_q expressions, and returns a new column
    //     populated with the then_q value if the condition is true, and the else_q value if the condition is false.
    // *)
    // and prepare_if_q (condition, then_q, else_q, ctx) =
    //     prepare (condition, ctx)
    //     ==> fun (cond, ctx) ->
    //             prepare (then_q, ctx)
    //             ==> fun (then_q, ctx) ->
    //                     prepare (else_q, ctx)
    //                     ==> fun (else_q, ctx) ->
    //                             match cond, then_q, else_q with
    //                             | [ c ], [ t ], [ e ] ->
    //                                 let mem_size = ctx.state.Head.Length
    //                                 let new_columns = [ mem_size ] // last column

    //                                 let new_state =
    //                                     seq {
    //                                         for row in ctx.state do
    //                                             row @ [ if row.[c] = (Bool true) then row.[t] else row.[e] ]
    //                                     }
    //                                     |> Seq.toList

    //                                 let ctx = { ctx with state = new_state }
    //                                 (new_columns, ctx) |> Ok
    //                             | _ -> $"Invalid inputs for ket if: {cond} then {then_q} else {else_q}" |> Error

    // (*
    //     First evaluates the condition, if the result is true then it prepares the then_q expression,
    //     otherwise it prepares the else_q expression. It returns the new column.
    // *)
    // and prepare_if_c (condition, then_q, else_q, ctx) =
    //     eval_classic (condition, ctx.evalCtx)
    //     ==> fun (cond, evalCtx) ->
    //             let ctx = { ctx with evalCtx = evalCtx }

    //             match cond with
    //             | (Bool true) -> prepare (then_q, ctx) ==> fun (then_q, ctx) -> (then_q, ctx) |> Ok
    //             | (Bool false) -> prepare (else_q, ctx) ==> fun (else_q, ctx) -> (else_q, ctx) |> Ok
    //             | _ -> $"Invalid classical input for if condition. Expecting bool, got {cond}" |> Error

    // (*
    //     Returns the columns of the specified ket.
    //  *)
    // and prepare_block (stmts, body, ctx) =
    //     eval_stmts (stmts, ctx.evalCtx)
    //     ==> fun evalCtx ->
    //             let ctx = { ctx with evalCtx = evalCtx }
    //             prepare (body, ctx)

    // (*
    //     Calls the corresponding method, and automatically prepares the resulting Ket
    // *)
    // and prepare_callmethod (method, args, ctx) =
    //     setup_method_body (method, args, ctx.evalCtx)
    //     ==> fun (body, argsCtx) ->
    //             match body with
    //             | Quantum (q, _) ->
    //                 let ctx' = { ctx with evalCtx = argsCtx }

    //                 prepare (q, ctx')
    //                 ==> fun (value, ctx') ->
    //                         // return the heap back to the original state
    //                         let ctx = { ctx' with evalCtx = ctx.evalCtx }
    //                         (value, ctx) |> Ok
    //             | _ -> $"Expecting a method with a Quantum body, got {method}" |> Error


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
                | Some (Value.Universe u) -> (Value.Universe u, evalCtx.graph) |> Ok
                | _ -> $"Invalid variable: {id}. Expecting universe." |> Error
            | U.Block (stmts, body) ->
                eval_stmts evalCtx stmts
                ==> fun evalCtx -> (this :> QPU).Prepare(body, evalCtx)
            | U.CallMethod (method, args) -> eval_callmethod evalCtx (method, args)
