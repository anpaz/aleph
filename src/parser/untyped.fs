namespace aleph.parser.ast

type Id = string

type Type =
    | Bool
    | Int
    | Tuple of Type list
    | Set of Type
    | Method of AnyType list * AnyType

and QType =
    | Ket of Type list

and AnyType =
    | Type of Type
    | QType of QType

type Aggregation =
    | Sum
    | And
    | Or

type Expression =
    | Var of Id
    | Bool of bool
    | Int of int
    | Tuple of values: Expression list
    | Set of values: Expression list
    | Range of start: Expression * stop: Expression

    | Method of arguments: (Id * AnyType) list * body: Expression
    | CallMethod of method: Expression * arguments: Expression list

    | Not of Expression
    | And of Expression * Expression
    | Or of Expression * Expression
    | Equals of Expression * Expression
    | LessThan of Expression * Expression

    | Add of Expression * Expression
    | Multiply of Expression * Expression
    | Join of Expression * Expression

    | Project of tuple: Expression * index: Expression list
    | Block of Statement list * Expression
    | If of cond: Expression * t : Expression * f: Expression
    | Summarize of id: Id * enumeration : Expression * aggregation: Aggregation * body: Expression

    | Ket of values: Expression list
    | KetAll of size: Expression

    | Sample of ket: Expression
    | Solve of ket: Expression * cond: Expression

and Statement =
    | Let of id: Id * value: Expression
    | Print of string * Expression list

