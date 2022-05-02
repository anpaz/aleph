namespace aleph.compiler.ast

type Expression<'E> =
    | Id of string
    | Bool of bool
    | Int of int
    | Tuple of values: Expression<'E> list
    | Set of values: Expression<'E> list
    | Range of start: Expression<'E> * stop: Expression<'E>

    | Not of Expression<'E>
    | Equals of Expression<'E> * Expression<'E>
    | LessThan of Expression<'E> * Expression<'E>

    | And of Expression<'E> list
    | Or of Expression<'E> list
    | Add of Expression<'E> list
    | Multiply of Expression<'E> list

    | Block of Statement<'E> list * Expression<'E>
    | If of cond: Expression<'E> * t : Expression<'E> * f: Expression<'E>
    | Summarize of id: string * enumeration : Expression<'E> * operation: string * body: Expression<'E>
    | Project of tuple: Expression<'E> * index: Expression<'E> list
    | CallMethod of id: string * arguments: Expression<'E> list

    // Quantum Extensions:
    | Q of 'E

    // // Quantum expressions
    // // TODO: | All
    // | Ket of values: Expression list
    // | Measure of ket: Expression
    // | Solve of ket: Expression
    // | CallQuantum of id: string * arguments: Expression list * ket: Expression

and Statement<'E> =
    | Let of id: string * value: Expression<'E>
    | Print of string * Expression<'E> list

