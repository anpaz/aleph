﻿namespace aleph.compiler.ast

type Expression =
    // Literals
    | Id of string
    | Bool of bool
    | Int of int
    | Tuple of values: Expression list
    | Set of values: Expression list
    | Range of start: Expression * stop: Expression
    | Ket of Expression

    // Bool expressions
    | Equals of Expression * Expression
    | LessThan of Expression * Expression
    | And of Expression * Expression
    | Or of Expression * Expression
    | Not of Expression 

    // Int expressions
    | Add of Expression * Expression
    | Multiply of Expression * Expression
    | Measure of ket: Expression

    // Tuple expressions
    | Item of tuple: Expression * index: Expression

    // Ket expressions
    | All
    | Solve of ket: Expression

    // Function calls
    | CallClassic of id: string * arguments: Expression list
    | CallQuantum of id: string * arguments: Expression list * ket: Expression

    // For debug
    | Message of string

type Statement =
    | Skip
    | Let of id: string * value: Expression
    | DefClassic of id: string * arguments: string list * body: Statement
    | DefQuantum of id: string * arguments: string list * ket : string * body: Statement
    | Return of Expression
    | If of cond: Expression * t: Statement * f: Statement
    | For of id: string * enumeration: Expression * body : Statement
    | Block of Statement list

    // For debugging:
    | Print of Expression

