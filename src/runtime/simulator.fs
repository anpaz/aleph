namespace aleph.runtime.simulator

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

type Memory = {
    allocations: Map<int, int list>
    state: Value list list
}

type Simulator() =
    let random = System.Random()
    let mutable max_ket = 0

    let mutable memory = { allocations = Map.empty; state = [] }

    (* 
        Preparing a Ket consists of first checking if the Ket is already allocated
        in memory, if so, it's a no-op
        otherwise, it prepares the state of the Ket's state prep expression
        and allocates in memory the columns returned by the state prep to this ket.
     *)
    let rec prepare (ket, ctx: ValueContext) =
        match memory.allocations.TryFind ket.Id with
        | Some _ -> 
            (ket, ctx) |> Ok        // Already prepared...
        | None ->
            prepare_state (ket.StatePrep, ctx)
            ==> fun (columns, ctx) ->
                // Assign to the ket the columns returned by the preparation:
                memory <- { memory with allocations = memory.allocations.Add (ket.Id, columns) }
                (ket, ctx) |> Ok

    (*
        Prepares the state with the given expression, and returns the index of the
        columns corresponding to the return value of the expression.
     *)
    and prepare_state (q, ctx) =
        match q with
        | Var id -> prepare_var (id, ctx)
        | Literal c -> prepare_literal (c, ctx)

        | Equals (left, right) -> prepare_equals (left, right, ctx)

        | Add (left, right) -> prepare_add (left, right, ctx)
        | Multiply (left, right) -> prepare_multiply (left, right, ctx)

        | Not q -> prepare_not (q, ctx)
        | And (left,right) -> prepare_and (left, right, ctx)
        | Or (left,right) -> prepare_or (left, right, ctx)

        | Project (q, indices) -> prepare_project (q, indices, ctx)
        | Join (left, right) -> prepare_join (left, right, ctx)
        | Solve (ket, condition) -> prepare_solve (ket, condition, ctx)
        | Block (stmts, value) -> prepare_block (stmts, value, ctx)

        | IfQuantum (condition, then_q, else_q) -> prepare_if_q (condition, then_q, else_q, ctx)
        | IfClassic (condition, then_q, else_q) -> prepare_if_c (condition, then_q, else_q, ctx)

        | Index _
        | KetAll _
        | Summarize _
        | CallMethod _ ->
            $"Not implemented: {q}" |> Error

    (*
        Finds the var as a Ket in the heap, and then calls prepare on the corresponding ket.
        After calling prepare on the ket it will be allocated in memory, as such
        this method returns the columns associated with the ket accordingly.
     *)
    and prepare_var (id, ctx) =
        match ctx.heap.TryFind id with
        | Some (Value.Ket ket) ->
            prepare (ket, ctx)
            ==> fun (ket, ctx) ->
                (memory.allocations.[ket.Id], ctx) |> Ok
        | _ ->
            $"Invalid variable: {id}. Expecting ket." |> Error

    (*
        Adding a new literal involves updating the quantum state 
        by doing the tensor product of the current state with the new values
        from the literal.
        It returns the columns allocated for the new values.
     *)
    and prepare_literal (values, ctx) =
        eval_classic(values, ctx)
        ==> fun (values, ctx) ->
            match values with
            | Value.Set values ->
                let old_size = if memory.state.IsEmpty then 0 else memory.state.Head.Length
                let new_state = tensort_product memory.state values
                let new_size = if new_state.IsEmpty then 0 else new_state.Head.Length
                let new_columns = [ old_size .. new_size - 1 ]
                memory <- { memory with state = new_state }
                (new_columns, ctx) |> Ok
            | _ -> 
                $"Invalid classic value for a ket literal: {values}" |> Error

    (*
        Adds a new column to the state, whose value is 
        the addition of the values in the columns from the corresponding input expressions.
        It returns the new column.
     *)
    and prepare_add (left, right, ctx) =
        prepare_state (left, ctx)
        ==> fun (left, ctx) ->
            prepare_state (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | ([l], [r]) ->
                    let mem_size = memory.state.Head.Length
                    let new_columns = [ mem_size ] // last column
                    let new_state = seq { for row in memory.state do row @ [ row.[l] + row.[r] ] } |> Seq.toList
                    memory <- { memory with state = new_state }
                    (new_columns, ctx) |> Ok
                | _ -> 
                    $"Invalid inputs for ket addition: {left} + {right}" |> Error

    (*
        Adds a new column to the state, whose value is 
        the multiplication of the values in the columns from the corresponding input expressions.
        It returns the new column.
     *)
    and prepare_multiply (left, right, ctx) =
        prepare_state (left, ctx)
        ==> fun (left, ctx) ->
            prepare_state (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | ([l], [r]) ->
                    let mem_size = memory.state.Head.Length
                    let new_columns = [ mem_size ] // last column
                    let new_state = seq { for row in memory.state do row @ [ row.[l] * row.[r] ] } |> Seq.toList
                    memory <- { memory with state = new_state }
                    (new_columns, ctx) |> Ok
                | _ -> 
                    $"Invalid inputs for ket addition: {left} + {right}" |> Error

    (*
        Returns the subset of the columns from the input expression given the 
        corresponding indices.
     *)
    and prepare_project (q, indices, ctx) =
        prepare_state (q, ctx)
        ==> fun (columns, ctx) ->
            let projection = 
                indices
                |> List.fold (fun result i -> result @ [ columns.[i] ]) []
            (projection, ctx) |> Ok

    (*
        Returns the concatenation of the results from the input expressions.
     *)
    and prepare_join (left, right, ctx) =
        prepare_state (left, ctx)
        ==> fun (left, ctx) ->
            prepare_state (right, ctx)
            ==> fun (right, ctx) ->
                (left @ right, ctx) |> Ok

    (*
        Adds a new column to the state, whose value is 
        the negation of the original column.
        It returns the new column.
     *)
    and prepare_not (q, ctx) =
        prepare_state (q, ctx)
        ==> fun (columns, ctx) ->
            match columns with
            | [v] ->
                let mem_size = memory.state.Head.Length
                let new_columns = [ mem_size ] // last column
                let new_state = seq { for row in memory.state do row @ [ (Value.Not row.[v]) ] } |> Seq.toList
                memory <- { memory with state = new_state }
                (new_columns, ctx) |> Ok
            | _ -> 
                $"Invalid inputs for ket not: {q}" |> Error

    (*
        Adds a new column to the state, whose value is 
        true iff the values in the columns from the corresponding input expressions are both true.
        It returns the new column.
     *)
    and prepare_and (left, right, ctx) =
        prepare_state (left, ctx)
        ==> fun (left, ctx) ->
            prepare_state (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | ([l], [r]) ->
                    let mem_size = memory.state.Head.Length
                    let new_columns = [ mem_size ] // last column
                    let new_state = seq { for row in memory.state do row @ [ Value.And (row.[l], row.[r]) ] } |> Seq.toList
                    memory <- { memory with state = new_state }
                    (new_columns, ctx) |> Ok
                | _ -> 
                    $"Invalid inputs for ket equals: {left} && {right}" |> Error

    (*
        Adds a new column to the state, whose value is 
        true iff the values in the columns from the corresponding input expressions are both true.
        It returns the new column.
     *)
    and prepare_or (left, right, ctx) =
        prepare_state (left, ctx)
        ==> fun (left, ctx) ->
            prepare_state (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | ([l], [r]) ->
                    let mem_size = memory.state.Head.Length
                    let new_columns = [ mem_size ] // last column
                    let new_state = seq { for row in memory.state do row @ [ Value.Or(row.[l], row.[r])      ] } |> Seq.toList
                    memory <- { memory with state = new_state }
                    (new_columns, ctx) |> Ok
                | _ -> 
                    $"Invalid inputs for ket equals: {left} && {right}" |> Error
    (*
        Adds a new column to the state, whose value is 
        true iff the values in the columns from the corresponding input expressions are equal.
        It returns the new column.
     *)
    and prepare_equals (left, right, ctx) =
        prepare_state (left, ctx)
        ==> fun (left, ctx) ->
            prepare_state (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | ([l], [r]) ->
                    let mem_size = memory.state.Head.Length
                    let new_columns = [ mem_size ] // last column
                    let new_state = seq { for row in memory.state do row @ [ row.[l] == row.[r] ] } |> Seq.toList
                    memory <- { memory with state = new_state }
                    (new_columns, ctx) |> Ok
                | _ -> 
                    $"Invalid inputs for ket equals: {left} == {right}" |> Error

    (*
        Filters the state to only those records that match the given condition.
        It returns the columns of the specified ket.
     *)
    and prepare_solve (ket, condition, ctx) =
        prepare_state (condition, ctx)
        ==> fun (cond, ctx) ->
            prepare_state (ket, ctx)
            ==> fun (ket, ctx) ->
                let new_state = List.filter (fun (r : Value list) -> r.[cond.Head] = (Bool true)) memory.state
                memory <- { memory with state = new_state }
                (ket, ctx) |> Ok

    (*
        Prepares all: the condition, the then_q and the else_q expressions, and returns a new column
        populated with the then_q value if the condition is true, and the else_q value if the condition is false.
    *)
    and prepare_if_q (condition, then_q, else_q, ctx) =
        prepare_state (condition, ctx)
        ==> fun (cond, ctx) ->
            prepare_state (then_q, ctx)
            ==> fun (then_q, ctx) ->
                prepare_state (else_q, ctx)
                ==> fun (else_q, ctx) ->
                    match cond, then_q, else_q with
                    | [c], [t], [e] ->
                        let mem_size = memory.state.Head.Length
                        let new_columns = [ mem_size ] // last column
                        let new_state = seq { for row in memory.state do row @ [ if row.[c] = (Bool true) then row.[t] else row.[e] ] } |> Seq.toList
                        memory <- { memory with state = new_state }
                        (new_columns, ctx) |> Ok
                    | _ -> 
                        $"Invalid inputs for ket if: {cond} then {then_q} else {else_q}" |> Error

    (*
        First evaluates the condition, if the result is true then it prepares the then_q expression,
        otherwise it prepares the else_q expression. It returns the new column.
    *)
    and prepare_if_c (condition, then_q, else_q, ctx) =
        eval_classic (condition, ctx)
        ==> fun (cond, ctx) ->
            match cond with
            | (Bool true) ->
                prepare_state (then_q, ctx)
                ==> fun (then_q, ctx) ->
                    (then_q, ctx) |> Ok
            | (Bool false) ->
                prepare_state (else_q, ctx)
                ==> fun (else_q, ctx) ->
                    (else_q, ctx) |> Ok
            | _ -> 
                $"Invalid classical input for if condition. Expecting bool, got {cond}" |> Error

    (*
        Returns the columns of the specified ket.
     *)
    and prepare_block (stmts, value, ctx) =
        eval_stmts (stmts, ctx)
        ==> fun ctx ->
            prepare_state (value, ctx)

    and tensort_product left right : Value list list =
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

    // exposes the current state of the simulator's memory. Mostly for testing purposes:
    member this.Memory = memory

    (*
        Implements the QPU interface used by the classical eval to interact with quantum
        expressions.
     *)
    interface QPU with
        (*
            Associate each expression with a Ket that has a unique id
         *)
        member this.Assign q =
            max_ket <- max_ket + 1
            { Id = max_ket; StatePrep = q }

        (*
            Resets memory. Cleans all allocations and state.
         *)
        member this.Reset(): unit = 
            memory <- { allocations = Map.empty; state = [] }

        (*
            Measure works by picking a random value with the same probability
            from the quantum state, and then projecting (selecting) only the columns
            associated with the ket.
         *)
        member this.Measure (ket: Ket) =
            let pick_random_value() =
                let i = int (random.NextDouble() * (double (memory.state.Length)))
                memory.state.Item i
            let project_ket_columns columns (all: Value list) =
                columns 
                |> List.fold (fun result i -> result @ [ all.[i] ]) []
                |> Tuple
            match memory.allocations.TryFind ket.Id with
            | Some columns ->
                pick_random_value()
                |> project_ket_columns columns
            | None -> failwith $"Ket: {ket.Id} not found in memory."

        (*
            Prepares the QPU's quantum state based on the given Ket.
         *)
        member this.Prepare (ket : Ket, ctx: ValueContext) = 
            assert (ctx.qpu = this)
            prepare (ket, ctx)
