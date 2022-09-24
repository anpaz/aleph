namespace aleph.parser.ast

type Id = string

type Type =
    | Bool
    | Int
    | Tuple of Type list
    | Set of Type
    | Method of Type list * Type
    | Ket of Type list
    | Universe of Type list

type Expression =
    | Var of Id
    | Bool of bool
    | Int of int
    | Tuple of values: Expression list
    | Set of values: Expression list
    | Method of arguments: (Id * Type) list * returns: Type * body: Expression
    | Range of start: Expression * stop: Expression

    | Not of Expression
    | And of Expression * Expression
    | Or of Expression * Expression
    | Equals of Expression * Expression
    | LessThan of Expression * Expression

    | Add of Expression * Expression
    | Multiply of Expression * Expression
    | Join of Expression * Expression

    | Project of tuple: Expression * index: Expression
    | Block of Statement list * Expression
    | If of cond: Expression * t : Expression * f: Expression

    | Ket of values: Expression
    | KetAll of size: Expression

    | CallMethod of method: Expression * arguments: Expression list

    | Sample of ket: Expression
    | Prepare of universe: Expression
    | Solve of ket: Expression * cond: Expression

    | Element of set: Expression
    | Append of item: Expression * set: Expression
    | Remove of item: Expression * set: Expression
    | Count of set: Expression


and Statement =
    | Let of id: Id * value: Expression
    | Print of string * Expression list

