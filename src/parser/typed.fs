namespace aleph.parser.ast.typed

open aleph.parser.ast

type C =
    | Var of Id

    | BoolLiteral of bool
    | IntLiteral of int
    | Tuple of values: C list
    | Set of values: C list
    | Range of start: C * stop: C

    | Not of C
    | And of C * C
    | Or of C * C
    | Equals of C * C
    | LessThan of C * C

    | Add of C * C
    | Multiply of C * C

    | Method of arguments: Id list * body: E

    | CallMethod of method: C * arguments: E list

    | Join of values: C * C
    | Project of source: C * index: int
    | Index of source: C * index: C
    | Block of stmts: Statement list * value: C
    | If of cond: C * t: C * f: C

    | Element of set: C
    | Append of item: C * set: C
    | Remove of item: C * set: C
    | Count of set: C

    | Sample of universe: U

and Q =
    | Var of Id

    | Literal of C
    | KetAll of size: C

    | Not of Q
    | And of Q * Q
    | Or of Q * Q
    | Equals of Q * Q

    | Add of Q * Q
    | Multiply of Q * Q

    | Join of values: Q * Q
    | Project of source: Q * indices: int
    | Index of source: Q * indices: C
    | Block of stmts: Statement list * Q
    | IfClassic of cond: C * t: Q * f: Q
    | IfQuantum of cond: Q * t: Q * f: Q

    | CallMethod of method: C * arguments: E list
    | Solve of ket: Q * condition: Q

and U =
    | Var of Id
    | CallMethod of method: C * arguments: E list
    | Block of stmts: Statement list * U
    | Prepare of ket: Q

and E =
    | Classic of C * Type
    | Quantum of Q * Type
    | Universe of U * Type

and Statement =
    | Let of id: Id * value: E
    | Print of string * E list
