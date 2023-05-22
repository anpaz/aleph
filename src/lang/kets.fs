namespace aleph

module kets =
    open System
    open aleph.utils
 
    type Expression =
        | Literal of width: int
        | Constant of value: int
        | Map of op: Operator * args: KetValue list
        | Where of target: KetValue * clause: Operator * args: KetValue list

    and Operator =
        | Not
        | LessThanEquals
        | Eq
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

    and KetValue(expression: Expression, ?id: KetId) =
        let id = match id with Some id -> id | None -> next_id()

        member this.Expression = expression
        member this.Id = id

    type IUniverse =
        interface end

    type QPU =
        abstract Prepare: KetValue list -> Result<IUniverse, string>
        abstract Measure: IUniverse * KetValue list-> Result<int list, string>

    type PrepareContext = { qpu : QPU }

    let ket (width: int) =
        KetValue(Literal (width=width))

    let prepare ctx (kets: KetValue list) : Result<IUniverse, string> =
        let qpu = ctx.qpu
        qpu.Prepare kets

    let sample ctx (kets: KetValue list) : Result<int list, string> =
        prepare ctx kets
        ==> fun u ->
            let qpu = ctx.qpu
            qpu.Measure (u, kets)

    let prepare_when ctx (kets: KetValue list, filter: KetValue) : Result<IUniverse, string> =
        prepare ctx (KetValue(Where (filter, Id, [])) :: kets)

    let sample_when ctx (kets: KetValue list, filter: KetValue) : Result<int list, string> =
        prepare_when ctx (kets, filter)
        ==> fun u ->
            let qpu = ctx.qpu
            qpu.Measure (u, kets)

    type KetValue with
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
                | Eq
                | In _ -> 1
            | Constant v -> int_width v
            | Where (target, _, _) -> target.Width

        member this.Where(op: Operator) = KetValue(Where(this, op, [ ]))

        member this.Where(op: Operator, arg: KetValue) = KetValue(Where(this, op, [ arg ]))

        member this.Where(op: Operator, c: int) = this.Where(op, KetValue(Constant c))

        member this.Where(op: Operator, b: bool) = this.Where(op, KetValue(Constant (if b then 1 else 0)))

        member this.Not() = KetValue(Map(Not, [ this ]))

        member this.In(items: int list) =
            KetValue(Map(In items, [ this ]))

        member this.And(k2: KetValue) = KetValue(Map(And, [ this; k2 ]))

        member this.And(c: bool) =
            let k2 = KetValue(Constant(if c then 1 else 0))
            this.And(k2)

        member this.Or(k2: KetValue) = KetValue(Map(Or, [ this; k2 ]))

        member this.Or(c: bool) =
            let k2 = KetValue(Constant(if c then 1 else 0))
            this.Or(k2)
            
        member this.LessThanEquals(k2: KetValue) = KetValue(Map(LessThanEquals, [ this; k2 ]))

        member this.LessThanEquals(c: int) =
            let k2 = KetValue(Constant(c))
            this.LessThanEquals(k2)

        member this.GreaterThan(k2: KetValue) = KetValue(Map(GreaterThan, [ this; k2 ]))

        member this.GreaterThan(c: int) =
            let k2 = KetValue(Constant(c))
            this.GreaterThan(k2)

        member this.Add(k2: KetValue, width: int) =
            KetValue(Map(Add width, [ this; k2 ]))

        member this.Add(k2: KetValue) =
            let w = Math.Max(this.Width, k2.Width)
            this.Add(k2, w)

        member this.Add(c: int) =
            let k2 = KetValue(Constant c)
            this.Add(k2)
            
        member this.Add(c: int, width: int) =
            let k2 = KetValue(Constant c)
            this.Add(k2, width)

        member this.Multiply(k2: KetValue, width: int) =
            KetValue(Map(Multiply width, [ this; k2 ]))

        member this.Multiply(k2: KetValue) =
            let w = this.Width + k2.Width
            this.Multiply(k2, w)

        member this.Multiply(c: int) =
            let k2 = KetValue(Constant c)
            this.Multiply(k2)
            
        member this.Multiply(c: int, width: int) =
            let k2 = KetValue(Constant c)
            this.Multiply(k2, width)

        member this.Equals(k2: KetValue) =
            KetValue(Map(Eq, [ this; k2 ]))

        member this.Equals(c: int) =
            let k2 = KetValue(Constant c)
            this.Equals(k2)

        member this.Equals(c: bool) =
            let k2 = KetValue(Constant(if c then 1 else 0))
            this.Equals(k2)

        member this.Choose(onTrue: KetValue, onFalse: KetValue) =
            KetValue(Map (If, [this; onTrue; onFalse]))
