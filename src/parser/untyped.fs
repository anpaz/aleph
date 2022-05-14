﻿namespace aleph.parser.ast

type Id = Id

type Expression =
    | Var of Id
    | Bool of bool
    | Int of int
    | Tuple of values: Expression list
    | Set of values: Expression list
    | Range of start: Expression * stop: Expression

    | Method of arguments: Id list * body: Expression
    | CallMethod of name: Id * arguments: Expression list

    | Not of Expression
    | And of Expression list
    | Or of Expression list
    | Equals of Expression * Expression
    | LessThan of Expression * Expression

    | Add of Expression list
    | Multiply of Expression list

    | Project of tuple: Expression * index: Expression list
    | Block of Statement list * Expression
    | If of cond: Expression * t : Expression * f: Expression
    | Summarize of id: Id * enumeration : Expression * operation: Id * body: Expression

    | Ket of values: Expression list
    | AllKet of size: int

    | QMethod of arguments: Id list * qarguments: Id list * body: Expression
    | CallQMethod of name: Id * arguments: Expression list * ket: Expression

    | Sample of ket: Expression
    | Measure of ket: Expression * shots: Expression
    | Solve of ket: Expression

and Statement =
    | Let of id: Id * value: Expression
    | Print of Id * Expression list

