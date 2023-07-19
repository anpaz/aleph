namespace aleph

module kets =
    open System
    open aleph.utils

    // ----------------
    // Data structures
    // ----------------
    type KetId = int

    and KetExpression =
        | Literal of width: int
        | Constant of value: int
        | Map of op: Operator * args: KetValue list
        | Where of target: KetValue * clause: Operator * args: KetValue list

    and Operator =
        | Id
        | Not
        | LessThanEquals
        | Eq
        | Add of width: int
        | Multiply of width: int
        | If

        // These were not in the paper
        | In of values: int list
        | GreaterThan
        | And
        | Or

    and KetValue(id: KetId, expression: KetExpression) =
        member this.Id = id
        member this.Expression = expression

    type IUniverse =
        interface
            abstract Sample: KetValue list -> Result<int list, string>
            abstract Histogram: KetValue list * int -> Result<Map<int list, int>, string>
        end

    type QPU =
        abstract Prepare: KetValue list -> Result<IUniverse, string>

    type PrepareContext = { qpu: QPU }

    // ----------------
    // Syntax
    // ----------------
    let ket (width: int) =
        KetValue(next_id (), Literal(width = width))

    let constant (v: int) = KetValue(next_id (), Constant(v))

    let map (op: Operator) (args: KetValue list) = KetValue(next_id (), Map(op, args))

    let where (ket: KetValue) (op: Operator) (args: KetValue list) =
        KetValue(next_id (), Where(ket, op, args))

    let prepare ctx (kets: KetValue list) : Result<IUniverse, string> =
        let qpu = ctx.qpu
        qpu.Prepare kets

    let sample ctx (kets: KetValue list) : Result<int list, string> =
        prepare ctx kets
        ==> fun u ->
            let qpu = ctx.qpu
            u.Sample(kets)

    let prepare_when ctx (kets: KetValue list) (filter: KetValue) : Result<IUniverse, string> =
        let w = where filter Id []
        prepare ctx (w :: kets)

    let sample_when ctx (kets: KetValue list) (filter: KetValue) : Result<int list, string> =
        prepare_when ctx kets filter ==> fun u -> u.Sample(kets)

    // ----------------
    // F# Extensions
    // ----------------
    type KetValue with

        member this.Width =
            match this.Expression with
            | Literal w -> w
            | Map(op, args) ->
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
            | Where(target, _, _) -> target.Width

        member this.Where(op: Operator) = where this op []

        member this.Where(op: Operator, arg: KetValue) = where this op [ arg ]

        member this.Where(op: Operator, c: int) = this.Where(op, constant c)

        member this.Where(op: Operator, b: bool) =
            this.Where(op, constant (if b then 1 else 0))

        member this.Not() = map Not [ this ]

        member this.In(items: int list) = map (In items) [ this ]

        member this.And(k2: KetValue) = map And [ this; k2 ]

        member this.And(c: bool) =
            let k2 = constant (if c then 1 else 0)
            this.And(k2)

        member this.Or(k2: KetValue) = map Or [ this; k2 ]

        member this.Or(c: bool) =
            let k2 = constant (if c then 1 else 0)
            this.Or(k2)

        member this.LessThanEquals(k2: KetValue) = map LessThanEquals [ this; k2 ]

        member this.LessThanEquals(c: int) =
            let k2 = constant (c)
            this.LessThanEquals(k2)

        member this.GreaterThan(k2: KetValue) = map GreaterThan [ this; k2 ]

        member this.GreaterThan(c: int) =
            let k2 = constant c
            this.GreaterThan(k2)

        member this.Add(k2: KetValue, width: int) = map (Add width) [ this; k2 ]

        member this.Add(k2: KetValue) =
            let w = Math.Max(this.Width, k2.Width) + 1
            this.Add(k2, w)

        member this.Add(c: int) =
            let k2 = constant c
            this.Add(k2)

        member this.Add(c: int, width: int) =
            let k2 = constant c
            this.Add(k2, width)

        member this.Multiply(k2: KetValue, width: int) = map (Multiply width) [ this; k2 ]

        member this.Multiply(k2: KetValue) =
            let w = this.Width + k2.Width
            this.Multiply(k2, w)

        member this.Multiply(c: int) =
            let k2 = constant c
            this.Multiply(k2)

        member this.Multiply(c: int, width: int) =
            let k2 = constant c
            this.Multiply(k2, width)

        member this.Equals(k2: KetValue) = map Eq [ this; k2 ]

        member this.Equals(c: int) =
            let k2 = constant c
            this.Equals(k2)

        member this.Equals(c: bool) =
            let k2 = constant (if c then 1 else 0)
            this.Equals(k2)

        member this.Choose(onTrue: KetValue, onFalse: KetValue) = map If [ this; onTrue; onFalse ]
