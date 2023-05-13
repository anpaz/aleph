namespace aleph.quals.parser.ast

type Expression =
    | Ket of weight: int
    | Constant of value:int
    | Map of op:Operator * args:Expression list
    | Where of target:Expression * clause : Operator * args:Expression list

and Operator =
    | IsZero
    | In of values: int list
    | LessThan 
    | Equals 
    | Add of weight: int
    | Multiply of  weight: int
    | If of weight: int
