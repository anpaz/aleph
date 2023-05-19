namespace aleph.qpu.classic

open aleph.utils
open aleph.kets

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

type ColumnsMap = Map<KetId, ColumnIndex>

type QuantumState(rows: int list list) =
    member val Rows = rows

    member this.AddColumn(values: int list) =
        if rows.IsEmpty then
            seq {
                for j in values do
                    [ j ]
            }
        else
            seq {
                for i in rows do
                    for j in values do
                        i @ [ j ]
            }
        |> Seq.toList 
        |> QuantumState

    member this.FilterRows(filter: ColumnIndex) =
        if rows.IsEmpty then 
            this
        else
            let check_empty (values: int list list) =
                if values.IsEmpty then
                    [ List.replicate rows.Head.Length (-1) ]
                else
                    values
            rows
            |> List.filter (fun r -> r.[filter] <> 0)
            |> check_empty
            |> QuantumState

    static member empty = [] |> QuantumState

type Universe(state: QuantumState, allocations: ColumnsMap) =
    let random = System.Random()
    let mutable value = None

    // Creates a row for this Universe with random values:
    let random_values columns =
        // Pick the max number of columns based on the columns of the output register of this universe.
        let max_column = columns |> List.max

        seq { for i in 0..max_column -> (random.Next()) } |> Seq.toList

    let project columns (row: int list) =
        columns |> List.fold (fun result i -> result @ [ row.[i] ]) []

    member val State = state

    (*
        Sample works by randomly picking a row from the universe with the same probability
        from the universe, and then projecting (selecting) only the columns
        associated with the ket.
        Once measured, the universe is collapsed to this value, and next time it is measured
        it will return the same value.
    *)
    member this.Sample(kets: Ket list) =
        let columns = kets |> List.map (fun k -> allocations.[k.Id])
        match value with
        | Some v -> v
        | None ->
            let pick_world () =
                match state.Rows.Length with
                // Universe collapsed:
                | 1 -> state.Rows.[0]
                // Empty universe, collapse to a row with random values
                | 0 -> random_values columns
                // Select a random row, and collapse to this value:
                | n ->
                    let i = int (random.NextDouble() * (double (n)))
                    state.Rows.Item i

            let world = pick_world ()
            value <- Some world
            world
        |> project columns

    override this.ToString() =
        sprintf "%A" state.Rows

    interface IUniverse

type QuantumContext =
    { state: QuantumState
      allocations: ColumnsMap }

type Processor() =

    let rec prepare ctx (ket: Ket) =
        match ctx.allocations.TryFind ket.Id with
        | Some _ -> ctx |> Ok // Already prepared...
        | None ->
            match ket.Expression with
            | Literal width -> prepare_literal ctx width
            | Constant value -> prepare_constant ctx value
            | Map (op, args) -> prepare_map ctx (op, args)
            | Where (target, op, args) -> prepare_where ctx (target, op, args)
            ==> fun (ctx, column) -> { ctx with allocations = ctx.allocations.Add(ket.Id, column) } |> Ok

    and prepare_many ctx kets =
        let init_one previous next = previous ==> fun ctx' -> prepare ctx' next
        kets |> List.fold init_one (Ok ctx)

    and prepare_literal ctx width =
        match width with
        | 0 -> "All literals must have a size." |> Error
        | n ->
            let values = seq { 0 .. (int (2.0 ** n)) - 1 } |> Seq.toList
            let new_state =  ctx.state.AddColumn values
            let new_column = new_state.Rows.Head.Length - 1
            ({ ctx with state = new_state }, new_column) |> Ok

    and prepare_constant ctx value =
        let new_state = ctx.state.AddColumn [ value ]
        let new_column = new_state.Rows.Head.Length - 1
        ({ ctx with state = new_state }, new_column) |> Ok

    and prepare_where ctx (target, op, args) =
        prepare_map ctx (op, target :: args)
        ==> fun (ctx, column) ->
            let ctx = { ctx with state = ctx.state.FilterRows column }
            (ctx, ctx.allocations.[target.Id]) |> Ok

    and prepare_map ctx (op, args) =
        prepare_many ctx args
        ==> fun ctx' ->
            match op with
            | Operator.Id -> (ctx', ctx'.allocations.[args.[0].Id]) |> Ok
            | Operator.Not -> map_unary ctx' (args.[0], (fun i -> if i = 0 then 1 else 0))
            | Operator.In items -> map_unary ctx' (args.[0], (fun i -> if items |> List.contains i then 1 else 0))
            | Operator.And ->
                map_binary ctx' (args.[0], args.[1], (fun (x, y) -> if (x = 1) && (y = 1) then 1 else 0))
            | Operator.Or ->
                map_binary ctx' (args.[0], args.[1], (fun (x, y) -> if (x = 1) || (y = 1) then 1 else 0))
            | Operator.LessThanEquals -> map_binary ctx' (args.[0], args.[1], (fun (x, y) -> if x <= y then 1 else 0))
            | Operator.GreaterThan -> map_binary ctx' (args.[0], args.[1], (fun (x, y) -> if x > y then 1 else 0))
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
        let new_column = ctx.state.Rows.Head.Length

        let new_state =
            seq {
                for row in ctx.state.Rows do
                    row @ [ lambda (row.[arg]) ]
            }
            |> Seq.toList

        ({ ctx with state = new_state |> QuantumState }, new_column) |> Ok

    and map_binary ctx (left: Ket, right: Ket, lambda: int * int -> int) =
        let x = ctx.allocations.[left.Id]
        let y = ctx.allocations.[right.Id]
        let new_column = ctx.state.Rows.Head.Length

        let new_state =
            seq {
                for row in ctx.state.Rows do
                    row @ [ lambda (row.[x], row.[y]) ]
            }
            |> Seq.toList

        ({ ctx with state = new_state |> QuantumState }, new_column) |> Ok

    and map_ternary ctx (a: Ket, b: Ket, c: Ket, lambda: int * int * int -> int) =
        let x = ctx.allocations.[a.Id]
        let y = ctx.allocations.[b.Id]
        let z = ctx.allocations.[c.Id]
        let new_column = ctx.state.Rows.Head.Length

        let new_state =
            seq {
                for row in ctx.state.Rows do
                    row @ [ lambda (row.[x], row.[y], row.[z]) ]
            }
            |> Seq.toList

        ({ ctx with state = new_state |> QuantumState }, new_column) |> Ok


    (*
        Implements the QPU interface used by the classical eval to interact with quantum
        expressions.
     *)
    interface QPU with
        (*
            Measure works by sampling the universe:
        *)
        member this.Measure(universe: IUniverse, kets: Ket list) =
            let u = universe :?> Universe
            u.Sample(kets) |> Ok

        (*
            Prepares a Quantum Universe from the given universe expression
         *)
        member this.Prepare(kets: Ket list) =
            let ctx = { allocations = Map.empty; state = QuantumState.empty }

            prepare_many ctx kets
            ==> fun ctx' ->
                Universe(ctx'.state, ctx'.allocations) :> IUniverse |> Ok


module context =

    let sample (kets: Ket list) =
        let ctx = { qpu = Processor()}
        sample ctx kets

    let sample_when (kets: Ket list, filter: Ket) =
        let ctx = { qpu = Processor()}
        sample_when ctx (kets, filter)