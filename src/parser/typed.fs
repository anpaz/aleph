namespace aleph.parser.typed

open aleph.parser.ast

type Type =
    | Bool
    | Int
    | Tuple of Type list
    | Set of Type
    | Histogram of Type
    | CMethod of Type list * Type
    | QMethod of Type list * QType * QType

and QType =
    | Ket of QType list
    | QBool
    | QInt

type C =
    | Var of string

    | BoolLiteral of bool
    | IntLiteral of int
    | Tuple of values: C list
    | Set of  values: C list
    | Range of  start: C * stop: C

    | Not of C
    | And of C list
    | Or of C list
    | Equals of C * C
    | LessThan of C * C

    | Add of C * C
    | Multiply of C * C

    | Method of name: Id * arguments: Id list * body: C
    | CallMethod of method: C * arguments: C list

    | Join of values: C * C
    | Project of source: C * indices: C list
    | Block of stmts: Statement list * value: C
    | If of cond: C * t : C * f: C
    | Summarize of id: Id * enumeration : C * operation: C * body: C

    | Sample of ket: Q
    | Measure of ket: Q * shots: C

and Q =
    | Var of string

    | Constant of C
    | AllKet of size: int

    | Not of Q
    | And of Q * Q
    | Or of Q * Q
    | Equals of Q * Q

    | Add of Q * Q
    | Multiply of Q * Q

    | Join of values: Q * Q
    | Project of source: Q * indices: C list
    | Block of qstmts: Statement list * Q
    | If of cond: Q * t : Q * f: Q
    | Summarize of id: Id * enumeration : C * operation: C * body: Q

    | CallQMethod of method: C * arguments: C list * qarguments: Q list
    | Solve of ket: Q

and E =
    | Classic of C * Type
    | Quantum of Q * QType

and Statement =
    | Let of id: Id * value: E
    | Print of Id * E list
