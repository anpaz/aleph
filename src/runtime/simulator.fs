namespace aleph.runtime.simulator

open aleph.parser.ast.typed
open aleph.runtime.Eval

(*

# QPU simulator

This class implements a classical simulator of a quantum processor.

The simulator works by representing the state of the quantum system as a matrix.
Each column of the matrix is a quantum register, each row represents a valid
combination of the tensor product across all registers. 

For example, preparing the state of two indepdent quantum registers
in which each can take values [0; 1] creates the matrix [0; 1] * [0; 1], i.e.:
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
                // Assigned to the ket the columns returned by the preparation:
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

        | Add (left, right) -> prepare_add (left, right, ctx)

        | Project (q, indices) -> prepare_project (q, indices, ctx)
        | Join (left, right) -> prepare_join (left, right, ctx)

        | KetAll _
        | Not _
        | And _
        | Or _
        | Equals _
        | Multiply _
        | Index _
        | Block _
        | IfClassic _
        | IfQuantum _
        | Summarize _
        | CallMethod _
        | Solve _ ->
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
                let new_columns = seq { old_size .. new_size - 1 } |> Seq.toList
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
            Prepares the give ket into memory.
         *)
        member this.Prepare (ket : Ket, ctx: ValueContext) = 
            assert (ctx.qpu = this)
            prepare (ket, ctx)
