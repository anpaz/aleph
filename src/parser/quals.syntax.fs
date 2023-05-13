namespace aleph.parser.quals.ast

type Expression =
    | Ket of weight: int
    | Constant of value:int
    | Map of operator:Operator * targets:Expression list
    | Where of target:Expression * clause : Operator * targets:Expression list

and Operator =
    | IsZero
    | In of values: int list
    | LessThan 
    | Equals 
    | Add of weight: int
    | Multiply of  weight: int
    | If of weight: int

// 
// Quick checks that the syntax can build the AST
//
module Tests =
    let plus = (Add 4)
    let times = (Multiply 8)

    let a = Ket(3)
    let b = Ket(3)

    let iszero = Map (IsZero, [a])
    let equals = Map (Equals, [a; iszero])
    let add = Map (plus, [a; b])
    let addi = Map (times, [a; Constant 3])
    let color = Where(a, LessThan, [Constant 3])
    let eq = Where(b, Equals, [a])
    let q = Map (If 3, [color; add; addi])
    let values = Where(Ket(3), In [0;2;4;6], [])
    let zero = Where(values, IsZero, [])