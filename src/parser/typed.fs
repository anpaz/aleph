namespace aleph.parser.typed

type Id = string

type Bool =
    | Literal of bool

    | Equals of Value * Value
    | Not of Bool
    | And of Bool * Bool
    | Or of Bool * Bool
    | LessThan of Int * Int

and Int =
    | Literal of int

    | Add of Int * Int
    | Multiply of Int * Int

and Tuple = 
    | Literal of Value * Value

    | Sample of Ket // Like measure, but only 1 shot.

and Set =
    | Literal of Tuple list
    | Range of Range

and Ket =
    | Literal of values: Set
    | AllKet of size: Int

    | Solve of Ket
    | CallQMethod of method: QMethod<Q> * arguments: C list * qarguments: Q list
    
and Histogram = 
    | Measure of ket: Ket * shots: Int

and Value = 
    | Bool of Bool
    | Int of Int
    | Tuple of Tuple
    | Set of Set
    | Ket of Ket

    | Join of Value * Value
    | Project of source: Value * index: Int

and C =
    | Value of Value
    | Histogram of Histogram
    | CMethod of CMethod<C>
    | QMethod of QMethod<Q>

    | Var of Id
    | MethodCall of CMethod<C>
    | Block of Block<C>
    | If of If<C>

and QBool =
    | Const of Bool
    | Equals of QValue * QValue
    | Not of QBool
    | And of QBool * QBool
    | Or of QBool * QBool

and QInt =
    | Const of Int
    | Add of QInt * QInt
    | Multiply of QInt * QInt

and QValue = 
    | QBool of QBool
    | QInt of QInt
    | Project of source: QValue * index: Int

and Q =
    | Ket of Ket
    | QBool of QBool
    | QInt of QInt

    | Var of Id
    | MethodCall of QMethod<Q>
    | Block of Block<Q>
    | If of If<Q>

and Expression =
    | Classical of C
    | Quantum of Q

and Statement =
    | Let of id : Id * value: Expression
    | Print of string * Expression list

and Block<'T> = { stmts : Statement list; value: 'T }
and If<'T> = { cond: Bool; t: 'T; f: 'T }
and Range = { start: Int; step: Int; stop: Int }
and CMethod<'T> = { arguments: Id list; body: 'T }
and QMethod<'T> = { arguments: Id list; qarguments: Id list; body: 'T }
