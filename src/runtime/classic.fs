namespace aleph.runtime

open aleph.compiler.ast
open aleph.runtime.Core

module Classic =

    type QuantumExpression =
    | Ket of values: Expression<QuantumExpression> list
    | Unitary of arguments: string list * qegs: string list * body: Expression<QuantumExpression>
    | CallUnitary of id: string * arguments: Expression<QuantumExpression> list * ket: Expression<QuantumExpression>
    | Measure of ket: Expression<QuantumExpression>
    | Solve of ket: Expression<QuantumExpression>
    | All

    type QuantumValue =
    | K         of SET
    | QMethod   of string list * string * Expression<QuantumExpression>

    type Value = Value<QuantumExpression, QuantumValue>
    type Context = Context<QuantumExpression, QuantumValue>
    type Expression = Expression<QuantumExpression>

    let rec evalQuantum (e, ctx) =
        match e with 
        | Ket _ 
        | Measure _
        | Solve _
        | Unitary _
        | CallUnitary _ 
        | All -> $"Not implemented: {e}" |> Error

    and eval = (evalCore<QuantumExpression, QuantumValue> evalQuantum)
