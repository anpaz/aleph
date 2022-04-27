namespace aleph.compiler.ast

type Expression =
    // Literals
    | Id of string
    | Bool of bool
    | Int of int
    | Tuple of values: Expression list
    | Set of values: Expression list
    | Ket of values: Expression list
    | Range of start: Expression * stop: Expression

    // Bool expressions
    | Equals of Expression * Expression
    | And of Expression list
    | Or of Expression list
    | Not of Expression 
    | LessThan of Expression * Expression

    // Int expressions
    | Add of Expression list
    | Multiply of Expression list

    // Tuple/Set expressions
    | Project of tuple: Expression * index: Expression list

    // Ket expressions
    | All
    | Measure of ket: Expression
    | Solve of ket: Expression

    // Function calls
    | CallClassic of id: string * arguments: Expression list
    | CallQuantum of id: string * arguments: Expression list * ket: Expression

    // For debug
    | Message of string

type Statement =
    | Skip
    | Block of Statement list
    | Return of Expression
    | Let of id: string * value: Expression
    | If of cond: Expression * t: Statement * f: Statement
    | For of id: string * enumeration: Expression * body : Statement
    | DefClassic of id: string * arguments: string list * body: Statement
    | DefQuantum of id: string * arguments: string list * ket : string * body: Statement

    // For debugging:
    | Print of Expression

