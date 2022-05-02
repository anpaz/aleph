namespace aleph.runtime

open aleph.compiler.ast
open aleph.runtime.Core

module Classic =

    type QuantumExpression =
    | Ket of values: Expression<QuantumExpression> list
    | Quantum of arguments: string list * qegs: string list * body: Expression<QuantumExpression>
    | CallQuantum of id: string * arguments: Expression<QuantumExpression> list * ket: Expression<QuantumExpression> list
    | Measure of ket: Expression<QuantumExpression>
    | Solve of ket: Expression<QuantumExpression>
    | All

    type QuantumValue =
    | K         of SET
    | QMethod   of string list * string * Expression<QuantumExpression>

    type Value = Value<QuantumExpression, QuantumValue>
    type Context = Context<QuantumExpression, QuantumValue>
    type Expression = Expression<QuantumExpression>

    let evalQuantum (e, ctx) =
        match e with 
        | All
        | Ket _ 
        | Measure _
        | Solve _
        | Quantum _
        | CallQuantum _ -> "Not implemented" |> Error

    let eval = (eval<QuantumExpression, QuantumValue> evalQuantum)
