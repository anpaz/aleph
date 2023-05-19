namespace aleph

module kets =
    open System
    open aleph.utils
 
    type Expression =
        | Literal of width: int
        | Constant of value: int
        | Map of op: Operator * args: Ket list
        | Where of target: Ket * clause: Operator * args: Ket list

    and Operator =
        | Not
        | LessThanEquals
        | Equals
        | Add of width: int
        | Multiply of width: int
        | If

        // These were not in the paper
        | Id
        | In of values: int list
        | GreaterThan
        | And
        | Or

    and KetId = int

    and Ket(expression: Expression, ?id: KetId) =
        let id = match id with Some id -> id | None -> next_id()
        let filterId = match expression with Where _ -> id + 1 |> Some | _ -> None

        member this.Expression = expression
        member this.FilterId = filterId
        member this.Id = id

    type IUniverse =
        interface end

    type QPU =
        abstract Prepare: Ket list -> Result<IUniverse, string>
        abstract Measure: IUniverse * Ket list-> Result<int list, string>

    type PrepareContext = { qpu : QPU }

    let ket (width: int) =
        Ket(Literal (width=width))

    let prepare ctx (kets: Ket list) : Result<IUniverse, string> =
        let qpu = ctx.qpu
        qpu.Prepare kets

    let sample ctx (kets: Ket list) : Result<int list, string> =
        prepare ctx kets
        ==> fun u ->
            let qpu = ctx.qpu
            qpu.Measure (u, kets)

    let sample_when ctx (kets: Ket list, filter: Ket) : Result<int list, string> =
        let f = Ket(Where (filter, Id, []))
        prepare ctx (f :: kets)
        ==> fun u ->
            let qpu = ctx.qpu
            qpu.Measure (u, kets)

    type Ket with
        member this.Width =
            match this.Expression with
            | Literal w -> w
            | Map (op, args) ->
                match op with
                | Add w
                | Multiply w -> w
                | If -> Math.Max(args.[1].Width, args.[2].Width)
                | Id -> args.[0].Width
                | Not
                | And
                | Or
                | LessThanEquals
                | GreaterThan
                | Equals
                | In _ -> 1
            | Constant v -> int_width v
            | Where (target, _, _) -> target.Width

        member this.Where(op: Operator) = Ket(Where(this, op, [ ]))

        member this.Where(op: Operator, arg: Ket) = Ket(Where(this, op, [ arg ]))

        member this.Where(op: Operator, c: int) = this.Where(op, Ket(Constant c))

        member this.Where(op: Operator, b: bool) = this.Where(op, Ket(Constant (if b then 1 else 0)))

        member this.Not() = Ket(Map(Not, [ this ]))

        member this.In(items: int list) =
            Ket(Map(In items, [ this ]))

        member this.And(k2: Ket) = Ket(Map(And, [ this; k2 ]))

        member this.And(c: bool) =
            let k2 = Ket(Constant(if c then 1 else 0))
            this.And(k2)

        member this.Or(k2: Ket) = Ket(Map(Or, [ this; k2 ]))

        member this.Or(c: bool) =
            let k2 = Ket(Constant(if c then 1 else 0))
            this.Or(k2)
            
        member this.LessThanEquals(k2: Ket) = Ket(Map(LessThanEquals, [ this; k2 ]))

        member this.LessThanEquals(c: int) =
            let k2 = Ket(Constant(c))
            this.LessThanEquals(k2)

        member this.GreaterThan(k2: Ket) = Ket(Map(GreaterThan, [ this; k2 ]))

        member this.GreaterThan(c: int) =
            let k2 = Ket(Constant(c))
            this.GreaterThan(k2)

        member this.Add(k2: Ket, width: int) =
            Ket(Map(Add width, [ this; k2 ]))

        member this.Add(k2: Ket) =
            let w = Math.Max(this.Width, k2.Width)
            this.Add(k2, w)

        member this.Add(c: int) =
            let k2 = Ket(Constant c)
            this.Add(k2)
            
        member this.Add(c: int, width: int) =
            let k2 = Ket(Constant c)
            this.Add(k2, width)

        member this.Multiply(k2: Ket, width: int) =
            Ket(Map(Multiply width, [ this; k2 ]))

        member this.Multiply(k2: Ket) =
            let w = this.Width + k2.Width
            this.Multiply(k2, w)

        member this.Multiply(c: int) =
            let k2 = Ket(Constant c)
            this.Multiply(k2)
            
        member this.Multiply(c: int, width: int) =
            let k2 = Ket(Constant c)
            this.Multiply(k2, width)

        member this.Equals(k2: Ket) =
            Ket(Map(Equals, [ this; k2 ]))

        member this.Equals(c: int) =
            let k2 = Ket(Constant c)
            this.Equals(k2)

        member this.Equals(c: bool) =
            let k2 = Ket(Constant(if c then 1 else 0))
            this.Equals(k2)

        member this.Choose(onTrue: Ket, onFalse: Ket) =
            Ket(Map (If, [this; onTrue; onFalse]))
