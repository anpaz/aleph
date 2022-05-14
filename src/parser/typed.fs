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

type TypedExpression =
    | Var of string * Type

    | BoolLiteral of bool * Type
    | IntLiteral of int * Type
    | Tuple of values: TypedExpression list * Type
    | Set of  values: TypedExpression list * Type
    | Range of  start: TypedExpression * stop: TypedExpression * Type

    | Not of TypedExpression * Type
    | And of TypedExpression * TypedExpression * Type
    | Or of TypedExpression * TypedExpression * Type
    | Equals of TypedExpression * TypedExpression * Type
    | LessThan of TypedExpression * TypedExpression * Type

    | Add of TypedExpression * TypedExpression * Type
    | Multiply of TypedExpression * TypedExpression * Type

    | Method of name: Id * arguments: Id list * body: TypedExpression * Type
    | CallMethod of method: TypedExpression * arguments: TypedExpression list * Type

    | Join of values: TypedExpression list * Type
    | Project of source: TypedExpression * indices: TypedExpression list * Type
    | Block of Statement list * value: TypedExpression * Type
    | If of cond: TypedExpression * t : TypedExpression * f: TypedExpression * Type
    | Summarize of id: Id * enumeration : TypedExpression * operation: TypedExpression * body: TypedExpression * Type

    | CallQMethod of method: TypedExpression * arguments: TypedExpression list * qarguments: QTypedExpression list * QType

    | Sample of ket: QTypedExpression * Type
    | Measure of ket: QTypedExpression * shots: TypedExpression * Type
    | Solve of ket: QTypedExpression * shots: TypedExpression * QType

and QTypedExpression =
    | Var of string * QType

    | Constant of TypedExpression * QType
    | AllKet of size: int * QType

    | Not of QTypedExpression * QType
    | And of QTypedExpression * QTypedExpression * QType
    | Or of QTypedExpression * QTypedExpression * QType
    | Equals of QTypedExpression * QTypedExpression * QType

    | Add of QTypedExpression * QTypedExpression * QType
    | Multiply of QTypedExpression * QTypedExpression * QType

    | Join of values: QTypedExpression list * QType
    | Project of source: QTypedExpression * indices: TypedExpression list * QType
    | Block of Statement list * QTypedExpression * QType
    | If of cond: QTypedExpression * t : QTypedExpression * f: QTypedExpression * QType
    | Summarize of id: Id * enumeration : TypedExpression * operation: TypedExpression * body: QTypedExpression * QType

    | CallQMethod of method: TypedExpression * arguments: TypedExpression list * qarguments: QTypedExpression list * QType
