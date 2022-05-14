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
    | Var of string * Type

    | BoolLiteral of bool * Type
    | IntLiteral of int * Type
    | Tuple of values: C list * Type
    | Set of  values: C list * Type
    | Range of  start: C * stop: C * Type

    | Not of C * Type
    | And of C * C * Type
    | Or of C * C * Type
    | Equals of C * C * Type
    | LessThan of C * C * Type

    | Add of C * C * Type
    | Multiply of C * C * Type

    | Method of name: Id * arguments: Id list * body: C * Type
    | CallMethod of method: C * arguments: C list * Type

    | Join of values: C list * Type
    | Project of source: C * indices: C list * Type
    | Block of stmts: Statement list * value: C * Type
    | If of cond: C * t : C * f: C * Type
    | Summarize of id: Id * enumeration : C * operation: C * body: C * Type

    | CallQMethod of method: C * arguments: C list * qarguments: Q list * QType

    | Sample of ket: Q * Type
    | Measure of ket: Q * shots: C * Type
    | Solve of ket: Q * shots: C * QType

and Q =
    | Var of string * QType

    | Constant of C * QType
    | AllKet of size: int * QType

    | Not of Q * QType
    | And of Q * Q * QType
    | Or of Q * Q * QType
    | Equals of Q * Q * QType

    | Add of Q * Q * QType
    | Multiply of Q * Q * QType

    | Join of values: Q list * QType
    | Project of source: Q * indices: C list * QType
    | Block of qstmts: Statement list * Q * QType
    | If of cond: Q * t : Q * f: Q * QType
    | Summarize of id: Id * enumeration : C * operation: C * body: Q * QType

    | CallQMethod of method: C * arguments: C list * qarguments: Q list * QType

and E =
    | Classic of C
    | Quantum of Q

and Statement =
    | Let of id: Id * value: E
    | Print of Id * E list
