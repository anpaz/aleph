namespace aleph.quals.parser.ast

open System

type Expression =
    | Literal of weight: int
    | Constant of value: int
    | Map of op: Operator * args: Ket list
    | Where of target: Ket * clause: Operator * args: Ket list

and Operator =
    | IsZero
    | LessThan
    | Equals
    | Add of weight: int
    | Multiply of weight: int
    | If of weight: int

    // These were not in the paper
    | In of values: int list
    | And
    | Or

and KetId = int

and Ket(expression: Expression) =
    static let ket_counter = ref 0
    let id : KetId = System.Threading.Interlocked.Increment(ket_counter) * 2
    let filterId = match expression with | Where _ -> id + 1 |> Some | _ -> None

    member this.Expression = expression

    member this.FilterId = filterId

    member this.Width =
        match this.Expression with
        | Literal w -> w
        | Map (op, _) ->
            match op with
            | IsZero
            | And
            | Or
            | LessThan
            | Equals
            | In _ -> 1
            | Add w
            | Multiply w
            | If w -> w
        | Constant v -> Math.Ceiling(Math.Log(float v) / Math.Log(2.0)) |> int
        | Where (target, _, _) -> target.Width

    member this.Id = id

    member this.Where(op: Operator, arg: Ket) = Ket(Where(this, op, [ arg ]))

    member this.Where(op: Operator, c: int) = this.Where(op, Ket(Constant c))

    member this.Where(op: Operator, b: bool) = this.Where(op, Ket(Constant (if b then 1 else 0)))

    member this.Not() = Ket(Map(IsZero, [ this ]))

    member this.And(k2: Ket) = Ket(Map(And, [ this; k2 ]))

    member this.And(c: bool) =
        let k2 = Ket(Constant(if c then 1 else 0))
        this.And(k2)

    member this.Or(k2: Ket) = Ket(Map(Or, [ this; k2 ]))

    member this.Or(c: bool) =
        let k2 = Ket(Constant(if c then 1 else 0))
        this.Or(k2)
        
    member this.LessThan(k2: Ket) = Ket(Map(LessThan, [ this; k2 ]))

    member this.LessThan(c: int) =
        let k2 = Ket(Constant(c))
        this.LessThan(k2)

    member this.Add(k2: Ket, weight: int) =
        Ket(Map(Add weight, [ this; k2 ]))

    member this.Add(k2: Ket) =
        let w = Math.Max(this.Width, k2.Width)
        this.Add(k2, w)

    member this.Add(c: int) =
        let k2 = Ket(Constant c)
        this.Add(k2)
        
    member this.Add(c: int, weight: int) =
        let k2 = Ket(Constant c)
        this.Add(k2, weight)

    member this.Multiply(k2: Ket, weigth: int) =
        Ket(Map(Multiply weigth, [ this; k2 ]))

    member this.Multiply(k2: Ket) =
        let w = Math.Max(this.Width, k2.Width)
        this.Multiply(k2, w)

    member this.Multiply(c: int) =
        let k2 = Ket(Constant c)
        this.Multiply(k2)
        
    member this.Multiply(c: int, weight: int) =
        let k2 = Ket(Constant c)
        this.Multiply(k2, weight)

    member this.Equals(k2: Ket) =
        let w = Math.Max(this.Width, k2.Width)
        Ket(Map(Equals, [ this; k2 ]))

    member this.Equals(c: int) =
        let k2 = Ket(Constant c)
        this.Equals(k2)

    member this.Equals(c: bool) =
        let k2 = Ket(Constant(if c then 1 else 0))
        this.Equals(k2)

    static member CollectFilterIds(kets: Ket list) : KetId list =
        let ket_filters (k: Ket) = 
            match k.Expression with
            | Map (_, args) -> Ket.CollectFilterIds args
            | Where (target, _, args) -> k.FilterId.Value :: Ket.CollectFilterIds(target :: args)
            | _ -> []

        kets |> List.collect (fun k -> ket_filters(k)) |> Set.ofList |> Set.toList
