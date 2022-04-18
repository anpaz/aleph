namespace aleph.compiler.ast

type Expression =
    | Add
    | Multiply
    | Range
    | Equals
    | Less_than
    | Not
    | Item
    | All


type Statement =
    | Skip
    | Let of string
    | Call
    | If
    | For

type Values =
    | Int of int
    | Boolean of bool
    | Tuple
    | Set of values: Set<Values>
    | Relation
    
