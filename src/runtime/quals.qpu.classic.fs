namespace aleph.quals.runtime.qpu.classic

open aleph.quals.parser.ast
open aleph.quals.runtime.Eval

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

type ColumnIndex = int

type ColumnsMap = Map<Expression, ColumnIndex>

type QuantumState = int list list

type Universe(state: QuantumState, filters: ColumnIndex list, outputColumns: ColumnIndex list) =
    let random = System.Random()
    let mutable value = None

    // Creates a row for this Universe with random values:
    let random_values () =
        // Pick the max number of columns based on the columns of the output register of this universe.
        let max_column =
            outputColumns |> List.max

        seq { for i in 0..max_column -> (random.Next()) } |> Seq.toList

    member val State = state
    member val Outputs = outputColumns
    member val Filters = filters

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
                // Empty universe, collapse to a row with random values
                | 0 -> random_values ()
                // Select a random row, and collapse to this value:
                | n ->
                    let i = int (random.NextDouble() * (double (n)))
                    state.Item i

            let sample = pick_world () |> (Universe.project outputColumns)
            value <- Some sample
            sample

    override this.ToString() =
        sprintf "%A" (seq { for i in state -> i |> (Universe.project outputColumns) } |> Seq.toList)

    static member project columns (row: int list) =
        columns |> List.fold (fun result i -> result @ [ row.[i] ]) []

    interface IUniverse

type QuantumContext =
    { state: QuantumState
      filters: ColumnIndex list
      allocations: ColumnsMap }

type Processor() =

    let rec init (ctx: QuantumContext) (k: Expression) =
        match ctx.allocations.TryFind k with
        | Some columns -> ctx |> Ok // Already prepared...
        | None ->
            let v = 
                match k with
                | Expression.Ket width ->
                    init_literal ctx width
                | Expression.Constant value ->
                    init_constant ctx value
                | Expression.Map (op, args) ->
                    init_map ctx (op, args)
                | Expression.Where (target, clause, args) ->
                    init_filter ctx (target, clause, args)
            v ==> fun (ctx', column) -> { ctx' with allocations = ctx'.allocations.Add(k, column) } |> Ok

    and init_many ctx expressions =
        let init_one (previous: Result<QuantumContext, string>) (next: Expression) =
            previous
            ==> fun ctx' -> init ctx' next
        expressions |> List.fold init_one (Ok ctx)

    and init_literal ctx width =
        match width with
        | 0 -> "All literals must have a size." |> Error
        | n -> 
            let values = seq { 0 .. (int (2.0 ** n)) - 1 } |> Seq.toList
            let new_state = add_values ctx.state values
            let new_column = new_state.Head.Length - 1
            ({ ctx with state = new_state }, new_column) |> Ok

    and init_constant ctx value =
        let new_state = add_values ctx.state [ value ]
        let new_column = new_state.Head.Length - 1
        ({ ctx with state = new_state }, new_column) |> Ok

    and init_map ctx (op, args) =
        init_many ctx args
        ==> fun ctx' ->
                match op with
                | Operator.IsZero -> 
                    map_unary ctx' (args.[0], (fun i -> if i = 0 then 1 else 0))
                | Operator.Not ->
                    map_unary ctx' (args.[0], (fun i -> if i = 1 then 0 else 1))
                | Operator.LessThan ->
                    map_binary ctx' (args.[0], args.[1], fun (x, y) -> if x < y then 1 else 0)
                | Operator.Equals ->
                    map_binary ctx' (args.[0], args.[1], fun (x, y) -> if x = y then 1 else 0)
                | Operator.Add w ->
                    let m = int (2.0 ** w)
                    map_binary ctx' (args.[0], args.[1], fun (x, y) -> (x + y) % m)
                | Operator.Multiply w ->
                    let m = int (2.0 ** w)
                    map_binary ctx' (args.[0], args.[1], fun (x, y) -> (x * y) % m)
                | In _
                | If _ ->
                    failwith "Not implemented" 

    and map_unary ctx (ket, lambda: int -> int) =
        let arg = ctx.allocations.[ket] 
        let new_column = ctx.state.Head.Length

        let new_state =
            seq {
                for row in ctx.state do
                    row @ [ lambda (row.[arg]) ]
            }
            |> Seq.toList

        ({ ctx with state = new_state }, new_column) |> Ok

    and map_binary ctx (left, right, lambda: int * int -> int) =
        let x = ctx.allocations.[left] 
        let y = ctx.allocations.[right]
        let new_column = ctx.state.Head.Length

        let new_state =
            seq {
                for row in ctx.state do
                    row @ [ lambda (row.[x], row.[y]) ]
            }
            |> Seq.toList

        ({ ctx with state = new_state }, new_column) |> Ok


    // and map_in ctx (ketId, values) =
    //     match v with
    //     | Value.Set s ->
    //         let columns = ctx.allocations.[ketId]
    //         let new_column = ctx.state.Head.Length

    //         let new_state =
    //             seq {
    //                 for row in ctx.state do
    //                     row @ [ Value.Bool(s.Contains(Universe.project columns row)) ]
    //             }
    //             |> Seq.toList

    //         ({ ctx with state = new_state }, ColumnIndex.One new_column) |> Ok
    //     | _ -> $"In map for preparation expects a set." |> Error

    // and map_if ctx ketId =
    //     match ctx.allocations.[ketId] with
    //     | ColumnIndex.Many [ c; t; e ] ->
    //         let new_column = ctx.state.Head.Length

    //         let new_state =
    //             seq {
    //                 for row in ctx.state do
    //                     row @ [ if row.[c] = (Bool true) then row.[t] else row.[e] ]
    //             }
    //             |> Seq.toList

    //         ({ ctx with state = new_state }, ColumnIndex.One new_column) |> Ok
    //     | error -> $"Invalid ket for if expression: {ketId} points to columns {error}." |> Error

    and init_filter ctx (op, clause, args) =
       failwith "Not implemented."

    and add_values (current: QuantumState) (right : int list) : QuantumState  =
        if current.IsEmpty then
            seq {
                for j in right do
                    [ j ]
            }  
        else
            seq {
                for i in current do
                    for j in right do
                        i @ [ j ]
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
        member this.Measure(universe: IUniverse) =
            let u = universe :?> Universe
            u.Sample() |> Ok

        (*
            Prepares a Quantum Universe from the given universe expression
         *)
        member this.Prepare(kets: Expression list) =
            let ctx =
                { allocations = Map.empty
                  filters = []
                  state = [] }

            init_many ctx kets
            ==> fun ctx' -> 
                let outputs = kets |> List.map (fun i -> ctx'.allocations.Item i)
                Universe(ctx'.state, ctx'.filters, outputs) :> IUniverse |> Ok
