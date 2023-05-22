module aleph.server.Utils

open aleph.kets

type Operator with
    member this.Label() =
        match this with
        | Operator.Not -> "not"
        | Operator.LessThanEquals -> "less than equal"
        | Operator.Eq -> "eq"
        | Operator.Add w -> $"add, width={w}"
        | Operator.Multiply w -> $"multiply, width={w}"
        | Operator.If -> "if"
        | Operator.Id -> "id"
        | Operator.In values -> "in"
        | Operator.GreaterThan -> "greater than"
        | Operator.And -> "and"
        | Operator.Or -> "or"

    static member Parse(label: string) =
        match label with
        | "eq" -> Operator.Eq
        | "not" -> Operator.Not
        | "lte" -> Operator.LessThanEquals
        | "if" -> Operator.If
        | "id" -> Operator.Id
        | "gt" -> Operator.GreaterThan
        | "and" -> Operator.And
        | _ -> failwith $"Invalid operator: {label} (does it require an argument?)"


type KetValue with
    member this.Label() =
        match this.Expression with
        | Expression.Literal w -> $"literal (width: {w})"
        | Expression.Constant v -> $"constant ({v})"
        | Expression.Map (op, _) -> $"map ({op.Label()})"
        | Expression.Where (target, clause, args) -> $"where (op: {clause.Label()})"