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

The state is prepared from one or more quantum expressions. Each expression
is assigned to a ket, which receives a unique id.
Each ket is prepared only once, as such the simulator memory keeps 
track of which kets have already been allocated by mapping the ket's id
to a list of columns.

*)

type ColumnIndex = int

type ColumnsMap = Map<int, ColumnIndex>

type QuantumState = int list list

type Universe(state: QuantumState, filters: ColumnIndex list, outputColumns: ColumnIndex list) =
    let random = System.Random()
    let mutable value = None

    // Creates a row for this Universe with random values:
    let random_values () =
        // Pick the max number of columns based on the columns of the output register of this universe.
        let max_column = outputColumns |> List.max

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
                let filtered_state: QuantumState = Universe.filter_rows (state, filters)

                match filtered_state.Length with
                // Universe collapsed:
                | 1 -> filtered_state.[0]
                // Empty universe, collapse to a row with random values
                | 0 -> random_values ()
                // Select a random row, and collapse to this value:
                | n ->
                    let i = int (random.NextDouble() * (double (n)))
                    filtered_state.Item i

            let world = pick_world ()
            let sample = world |> (Universe.project outputColumns)
            value <- Some sample
            sample

    override this.ToString() =
        sprintf "%A" (seq { for i in state -> i |> (Universe.project outputColumns) } |> Seq.toList)

    static member add_column(state: QuantumState, values: int list) =
        if state.IsEmpty then
            seq {
                for j in values do
                    [ j ]
            }
        else
            seq {
                for i in state do
                    for j in values do
                        i @ [ j ]
            }
        |> Seq.toList

    static member filter_rows(state: QuantumState, filters: ColumnIndex list) =
        if filters.IsEmpty then
            state
        else
            let rec is_valid columns (row: ColumnIndex list) =
                match columns with
                | [] -> true
                | [ i ] -> row.[i] = 1
                | head :: tail -> if row.[head] = 0 then false else is_valid tail row

            state |> List.filter (is_valid filters)

    static member project columns (row: int list) =
        columns |> List.fold (fun result i -> result @ [ row.[i] ]) []

    interface IUniverse

type QuantumContext =
    { state: QuantumState
      allocations: ColumnsMap }

type Processor() =

    let rec init (ctx: QuantumContext) (k: Ket) =
        match ctx.allocations.TryFind k.Id with
        | Some _ -> ctx |> Ok // Already prepared...
        | None ->
            let init_result =
                match k.Expression with
                | Literal width -> init_literal ctx width
                | Constant value -> init_constant ctx value
                | Map (op, args) -> init_map ctx (op, args)
                | Where (target, op, args) ->
                    init_map ctx (op, target :: args)
                    ==> fun (ctx, column) ->
                            let ctx = { ctx with allocations = ctx.allocations.Add(k.FilterId.Value, column) }
                            (ctx, ctx.allocations.[target.Id]) |> Ok

            init_result
            ==> fun (ctx, column) -> { ctx with allocations = ctx.allocations.Add(k.Id, column) } |> Ok

    and init_many ctx expressions =
        let init_one previous next = previous ==> fun ctx' -> init ctx' next
        expressions |> List.fold init_one (Ok ctx)

    and init_literal ctx width =
        match width with
        | 0 -> "All literals must have a size." |> Error
        | n ->
            let values = seq { 0 .. (int (2.0 ** n)) - 1 } |> Seq.toList
            let new_state = Universe.add_column (ctx.state, values)
            let new_column = new_state.Head.Length - 1
            ({ ctx with state = new_state }, new_column) |> Ok

    and init_constant ctx value =
        let new_state = Universe.add_column (ctx.state, [ value ])
        let new_column = new_state.Head.Length - 1
        ({ ctx with state = new_state }, new_column) |> Ok

    and init_map ctx (op, args) =
        init_many ctx args
        ==> fun ctx' ->
                match op with
                | Operator.Not -> map_unary ctx' (args.[0], (fun i -> if i = 0 then 1 else 0))
                | Operator.In items -> map_unary ctx' (args.[0], (fun i -> if items |> List.contains i then 1 else 0))
                | Operator.And ->
                    map_binary ctx' (args.[0], args.[1], (fun (x, y) -> if (x = 1) && (y = 1) then 1 else 0))
                | Operator.Or ->
                    map_binary ctx' (args.[0], args.[1], (fun (x, y) -> if (x = 1) || (y = 1) then 1 else 0))
                | Operator.LessThanEquals -> map_binary ctx' (args.[0], args.[1], (fun (x, y) -> if x <= y then 1 else 0))
                | Operator.Equals -> map_binary ctx' (args.[0], args.[1], (fun (x, y) -> if x = y then 1 else 0))
                | Operator.Add w ->
                    let m = int (2.0 ** w)
                    map_binary ctx' (args.[0], args.[1], (fun (x, y) -> (x + y) % m))
                | Operator.Multiply w ->
                    let m = int (2.0 ** w)
                    map_binary ctx' (args.[0], args.[1], (fun (x, y) -> (x * y) % m))
                | Operator.If w ->
                    let m = int (2.0 ** w)
                    map_ternary ctx' (args.[0], args.[1], args.[2], (fun (x, y, z) -> if x = 0 then z % m else y % m))

    and map_unary ctx (ket: Ket, lambda: int -> int) =
        let arg = ctx.allocations.[ket.Id]
        let new_column = ctx.state.Head.Length

        let new_state =
            seq {
                for row in ctx.state do
                    row @ [ lambda (row.[arg]) ]
            }
            |> Seq.toList

        ({ ctx with state = new_state }, new_column) |> Ok

    and map_binary ctx (left: Ket, right: Ket, lambda: int * int -> int) =
        let x = ctx.allocations.[left.Id]
        let y = ctx.allocations.[right.Id]
        let new_column = ctx.state.Head.Length

        let new_state =
            seq {
                for row in ctx.state do
                    row @ [ lambda (row.[x], row.[y]) ]
            }
            |> Seq.toList

        ({ ctx with state = new_state }, new_column) |> Ok

    and map_ternary ctx (a: Ket, b: Ket, c: Ket, lambda: int * int * int -> int) =
        let x = ctx.allocations.[a.Id]
        let y = ctx.allocations.[b.Id]
        let z = ctx.allocations.[c.Id]
        let new_column = ctx.state.Head.Length

        let new_state =
            seq {
                for row in ctx.state do
                    row @ [ lambda (row.[x], row.[y], row.[z]) ]
            }
            |> Seq.toList

        ({ ctx with state = new_state }, new_column) |> Ok


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
        member this.Prepare(kets: Ket list) =
            let ctx = { allocations = Map.empty; state = [] }

            init_many ctx kets
            ==> fun ctx' ->
                    let outputs = kets |> List.map (fun i -> ctx'.allocations.Item i.Id)

                    let filters =
                        Ket.CollectFilterIds(kets) |> List.map (fun i -> ctx'.allocations.Item i)

                    Universe(ctx'.state, filters, outputs) :> IUniverse |> Ok
