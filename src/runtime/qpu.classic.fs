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


type QuantumContext = {
    allocations: Map<int, int list>
    state: Value list list
    evalCtx: EvalContext
}

type Universe(state: Value list list, columns: int list) =
    let random = System.Random()
    let mutable value = None

    interface IUniverse with
        member this.CompareTo(obj: obj): int = 
            failwith "Not Implemented"

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
        | Some v ->
            v
        | None ->
            let pick_world() =
                match state.Length with
                // Universe collapsed:
                | 1 -> 
                    state.[0]
                // Empty universe, collapse to random value
                | 0 -> 
                    let row = 
                        if columns.IsEmpty then 
                            []
                        else 
                            seq { for i in 0 .. (columns |> List.max) -> (Value.Int (random.Next())) }  |> Seq.toList
                    row
                // Select a random row, and collapse to this value:
                | n -> 
                    let i = int (random.NextDouble() * (double (n)))
                    state.Item i
            let project_columns (row: Value list) =
                if columns.Length = 1 then
                    row.[columns.[0]]
                else
                    columns 
                    |> List.fold (fun result i -> result @ [ row.[i] ]) []
                    |> Tuple
            let sample = pick_world() |> project_columns
            value <- Some sample
            sample

type Processor() =

    (* 
        Preparing a Ket consists of first checking if the Ket is already allocated
        in memory, if so, it's a no-op
        otherwise, it prepares the state of the Ket's state prep expression
        and allocates in memory the columns returned by the state prep to this ket.
     *)
    let rec prepare_ket (ket : Ket, ctx: QuantumContext) =
        match ctx.allocations.TryFind ket.Id with
        | Some columns -> 
            (columns, ctx) |> Ok        // Already prepared...
        | None ->
            prepare (ket.StatePrep, ctx)
            ==> fun (columns, ctx) ->
                // Assign to the ket the columns returned by the preparation:
                let ctx = { ctx with allocations = ctx.allocations.Add (ket.Id, columns) }
                (columns, ctx) |> Ok

    (*
        Prepares the Universe for the given expression, and returns the index of the
        columns corresponding to the return value of the expression.
     *)
    and prepare (q, ctx) =
        match q with
        | Q.Var id -> prepare_var (id, ctx)
        | Literal c -> prepare_literal (c, ctx)
        | KetAll size -> prepare_ketall (size, ctx)

        | Equals (left, right) -> prepare_equals (left, right, ctx)

        | Add (left, right) -> prepare_add (left, right, ctx)
        | Multiply (left, right) -> prepare_multiply (left, right, ctx)

        | Not q -> prepare_not (q, ctx)
        | And (left,right) -> prepare_and (left, right, ctx)
        | Or (left,right) -> prepare_or (left, right, ctx)

        | Project (q, index) -> prepare_project (q, index, ctx)
        | Index (q, index) -> prepare_index (q, index, ctx)
        | Join (left, right) -> prepare_join (left, right, ctx)
        | Solve (ket, condition) -> prepare_solve (ket, condition, ctx)
        | Q.Block (stmts, value) -> prepare_block (stmts, value, ctx)

        | IfQuantum (condition, then_q, else_q) -> prepare_if_q (condition, then_q, else_q, ctx)
        | IfClassic (condition, then_q, else_q) -> prepare_if_c (condition, then_q, else_q, ctx)

        | Q.CallMethod (method, args) ->  prepare_callmethod(method, args, ctx)

        | Summarize _ ->
            $"`Not implemented: {q}" |> Error


    (*
        Finds the var as a Ket in the heap, and then calls prepare on the corresponding ket.
        After calling prepare on the ket it will be allocated in memory, as such
        this method returns the columns associated with the ket accordingly.
     *)
    and prepare_var (id, ctx) =
        match ctx.evalCtx.heap.TryFind id with
        | Some (Value.Ket ket) ->
            prepare_ket (ket, ctx)
            ==> fun (columns, ctx) ->
                (columns, ctx) |> Ok
        | _ ->
            $"Invalid variable: {id}. Expecting ket." |> Error

    (*
        Adding a new literal involves updating the quantum state 
        by doing the tensor product of the current state with the new values
        from the literal.
        It returns the columns allocated for the new values.
     *)
    and prepare_literal (values, ctx) =
        eval_classic(values, ctx.evalCtx)
        ==> fun (values, evalCtx) ->
            match values with
            | Value.Set values ->
                let old_size = if ctx.state.IsEmpty then 0 else ctx.state.Head.Length
                let new_state = tensor_product ctx.state (values |> Set.toList)
                let new_size = if new_state.IsEmpty then 0 else new_state.Head.Length
                let new_columns = [ old_size .. new_size - 1 ]
                let ctx = { ctx with state = new_state; evalCtx = evalCtx }
                (new_columns, ctx) |> Ok
            | _ -> 
                $"Invalid classic value for a ket literal: {values}" |> Error

    and prepare_ketall (size, ctx) =
        eval_classic (size, ctx.evalCtx)
        ==> fun (size, evalCtx) ->
            match size with
            | Value.Int i ->
                let values = seq { 0 .. (int(2.0 ** i)) - 1} |> Seq.map (Value.Int) |> Seq.toList
                let old_size = if ctx.state.IsEmpty then 0 else ctx.state.Head.Length
                let new_state = tensor_product ctx.state values
                let new_size = if new_state.IsEmpty then 0 else new_state.Head.Length
                let new_columns = [ old_size .. new_size - 1 ]
                let ctx = { ctx with state = new_state; evalCtx = evalCtx }
                (new_columns, ctx) |> Ok
            | _ -> 
                $"Invalid ket_all size, expected int got: {size}" |> Error
    (*
        Adds a new column to the state, whose value is 
        the addition of the values in the columns from the corresponding input expressions.
        It returns the new column.
     *)
    and prepare_add (left, right, ctx) =
        prepare (left, ctx)
        ==> fun (left, ctx) ->
            prepare (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | ([l], [r]) ->
                    let mem_size = ctx.state.Head.Length
                    let new_columns = [ mem_size ] // last column
                    let new_state = seq { for row in ctx.state do row @ [ row.[l] + row.[r] ] } |> Seq.toList
                    let ctx = { ctx with state = new_state }
                    (new_columns, ctx) |> Ok
                | _ -> 
                    $"Invalid inputs for ket addition: {left} + {right}" |> Error

    (*
        Adds a new column to the state, whose value is 
        the multiplication of the values in the columns from the corresponding input expressions.
        It returns the new column.
     *)
    and prepare_multiply (left, right, ctx) =
        prepare (left, ctx)
        ==> fun (left, ctx) ->
            prepare (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | ([l], [r]) ->
                    let mem_size = ctx.state.Head.Length
                    let new_columns = [ mem_size ] // last column
                    let new_state = seq { for row in ctx.state do row @ [ row.[l] * row.[r] ] } |> Seq.toList
                    let ctx = { ctx with state = new_state }
                    (new_columns, ctx) |> Ok
                | _ -> 
                    $"Invalid inputs for ket addition: {left} + {right}" |> Error

    (*
        Returns the the column from the input expression corresponding to the given index.
     *)
    and prepare_project (q, index, ctx) =
        prepare (q, ctx)
        ==> fun (columns, ctx) ->
            let projection = [ columns.[index] ]
            (projection, ctx) |> Ok

    (*
        Evaluates the index expression and returns the corresponding column.
     *)
    and prepare_index (q, index, ctx) =
        prepare (q, ctx)
        ==> fun (columns, ctx) ->
            eval_classic (index, ctx.evalCtx)
            ==> fun (index, evalCtx) ->
                match index with
                | Value.Int i ->
                    let ctx = { ctx with evalCtx = evalCtx }
                    let idx = i % columns.Length
                    ([columns.[idx]], ctx) |> Ok
                | _ -> $"Invalid index, expecting int value, got {index}" |> Error

    (*
        Returns the concatenation of the results from the input expressions.
     *)
    and prepare_join (left, right, ctx) =
        prepare (left, ctx)
        ==> fun (left, ctx) ->
            prepare (right, ctx)
            ==> fun (right, ctx) ->
                (left @ right, ctx) |> Ok

    (*
        Adds a new column to the state, whose value is 
        the negation of the original column.
        It returns the new column.
     *)
    and prepare_not (q, ctx) =
        prepare (q, ctx)
        ==> fun (columns, ctx) ->
            match columns with
            | [v] ->
                let mem_size = ctx.state.Head.Length
                let new_columns = [ mem_size ] // last column
                let new_state = seq { for row in ctx.state do row @ [ (Value.Not row.[v]) ] } |> Seq.toList
                let ctx = { ctx with state = new_state }
                (new_columns, ctx) |> Ok
            | _ -> 
                $"Invalid inputs for ket not: {q}" |> Error

    (*
        Adds a new column to the state, whose value is 
        true iff the values in the columns from the corresponding input expressions are both true.
        It returns the new column.
     *)
    and prepare_and (left, right, ctx) =
        prepare (left, ctx)
        ==> fun (left, ctx) ->
            prepare (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | ([l], [r]) ->
                    let mem_size = ctx.state.Head.Length
                    let new_columns = [ mem_size ] // last column
                    let new_state = seq { for row in ctx.state do row @ [ Value.And (row.[l], row.[r]) ] } |> Seq.toList
                    let ctx = { ctx with state = new_state }
                    (new_columns, ctx) |> Ok
                | _ -> 
                    $"Invalid inputs for ket equals: {left} && {right}" |> Error

    (*
        Adds a new column to the state, whose value is 
        true iff the values in the columns from the corresponding input expressions are both true.
        It returns the new column.
     *)
    and prepare_or (left, right, ctx) =
        prepare (left, ctx)
        ==> fun (left, ctx) ->
            prepare (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | ([l], [r]) ->
                    let mem_size = ctx.state.Head.Length
                    let new_columns = [ mem_size ] // last column
                    let new_state = seq { for row in ctx.state do row @ [ Value.Or(row.[l], row.[r])      ] } |> Seq.toList
                    let ctx = { ctx with state = new_state }
                    (new_columns, ctx) |> Ok
                | _ -> 
                    $"Invalid inputs for ket equals: {left} && {right}" |> Error

    (*
        Adds a new column to the state, whose value is 
        true iff the values in the columns from the corresponding input expressions are equal.
        It returns the new column.
     *)
    and prepare_equals (left, right, ctx) =
        prepare (left, ctx)
        ==> fun (left, ctx) ->
            prepare (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | ([l], [r]) ->
                    let mem_size = ctx.state.Head.Length
                    let new_columns = [ mem_size ] // last column
                    let new_state = seq { for row in ctx.state do row @ [ row.[l] == row.[r] ] } |> Seq.toList
                    let ctx = { ctx with state = new_state }
                    (new_columns, ctx) |> Ok
                | _ -> 
                    $"Invalid inputs for ket equals: {left} == {right}" |> Error

    (*
        Filters the state to only those records that match the given condition.
        It returns the columns of the specified ket.
     *)
    and prepare_solve (ket, condition, ctx) =
        prepare (condition, ctx)
        ==> fun (cond, ctx) ->
            prepare (ket, ctx)
            ==> fun (ket, ctx) ->
                let new_state = List.filter (fun (r : Value list) -> r.[cond.Head] = (Bool true)) ctx.state
                let ctx = { ctx with state = new_state }
                (ket, ctx) |> Ok

    (*
        Prepares all: the condition, the then_q and the else_q expressions, and returns a new column
        populated with the then_q value if the condition is true, and the else_q value if the condition is false.
    *)
    and prepare_if_q (condition, then_q, else_q, ctx) =
        prepare (condition, ctx)
        ==> fun (cond, ctx) ->
            prepare (then_q, ctx)
            ==> fun (then_q, ctx) ->
                prepare (else_q, ctx)
                ==> fun (else_q, ctx) ->
                    match cond, then_q, else_q with
                    | [c], [t], [e] ->
                        let mem_size = ctx.state.Head.Length
                        let new_columns = [ mem_size ] // last column
                        let new_state = seq { for row in ctx.state do row @ [ if row.[c] = (Bool true) then row.[t] else row.[e] ] } |> Seq.toList
                        let ctx = { ctx with state = new_state }
                        (new_columns, ctx) |> Ok
                    | _ -> 
                        $"Invalid inputs for ket if: {cond} then {then_q} else {else_q}" |> Error

    (*
        First evaluates the condition, if the result is true then it prepares the then_q expression,
        otherwise it prepares the else_q expression. It returns the new column.
    *)
    and prepare_if_c (condition, then_q, else_q, ctx) =
        eval_classic (condition, ctx.evalCtx)
        ==> fun (cond, evalCtx) ->
            let ctx = { ctx with evalCtx = evalCtx }
            match cond with
            | (Bool true) ->
                prepare (then_q, ctx)
                ==> fun (then_q, ctx) ->
                    (then_q, ctx) |> Ok
            | (Bool false) ->
                prepare (else_q, ctx)
                ==> fun (else_q, ctx) ->
                    (else_q, ctx) |> Ok
            | _ -> 
                $"Invalid classical input for if condition. Expecting bool, got {cond}" |> Error

    (*
        Returns the columns of the specified ket.
     *)
    and prepare_block (stmts, value, ctx) =
        eval_stmts (stmts, ctx.evalCtx)
        ==> fun evalCtx ->
            let ctx = { ctx with evalCtx = evalCtx }
            prepare (value, ctx)

    (*
        Calls the corresponding method, and automatically prepares the resulting Ket
    *)
    and prepare_callmethod (method, args, ctx) =
        eval_callmethod(method, args, ctx.evalCtx)
        ==> fun (value, evalCtx) ->
            match value with
            | Value.Ket k -> 
                let ctx = { ctx with evalCtx = evalCtx }
                prepare_ket (k, ctx)
            | _ ->
                $"Expecting a Ket result, got {value}" |> Error

    and tensor_product left right : Value list list =
        let as_list = function
            | Value.Bool b -> [ Value.Bool b ]
            | Value.Int i -> [ Value.Int i ]
            | Value.Tuple t -> t
            | _ -> failwith "Invalid tuple value"
        if left.IsEmpty then
            seq { for j in right -> (j |> as_list) } 
        else
            seq { for i in left do for j in right -> i @ (j |> as_list) }
        |> Seq.toList 

    (*
        Implements the QPU interface used by the classical eval to interact with quantum
        expressions.
     *)
    interface QPU with
        (*
            Measure works by sampling the universe:
        *)
        member this.Measure (universe: IUniverse) =
            let u = universe :?> Universe
            u.Sample() |> Ok

        (*
            Prepares a Quantum Universe from the given universe expression
         *)
        member this.Prepare (u, evalCtx: EvalContext) = 
            assert (evalCtx.qpu = this)
            match u with
            | U.Prepare q ->
                eval_quantum (q, evalCtx)
                ==> fun (ket, evalCtx) ->
                    match ket with
                    | Value.Ket ket -> 
                        let ctx = { 
                            allocations = Map.empty; 
                            state = []; 
                            evalCtx = evalCtx }
                        prepare_ket (ket, ctx)
                        ==> fun (columns, ctx) ->
                            (Value.Universe (Universe(ctx.state, columns)), ctx.evalCtx) |> Ok
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
