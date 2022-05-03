namespace aleph.runtime

open aleph.parser.core
open aleph.parser.quantum

open aleph.runtime.Core

module Classic =

    type QuantumValue =
    | Ket         of SET
    | Unitary     of string list * string * Expression<QuantumExpression>

    type Value = Value<QuantumExpression, QuantumValue>
    type Context = Context<QuantumExpression, QuantumValue>
    type Expression = Expression<QuantumExpression>

    let rec evalQuantum (e: QuantumExpression, ctx) =
        match e with 
        | QuantumExpression.Ket _ 
        | QuantumExpression.Measure _
        | QuantumExpression.Solve _
        | QuantumExpression.Unitary _
        | QuantumExpression.CallUnitary _ 
        | QuantumExpression.All -> $"Not implemented: {e}" |> Error

    and private evalKet (values, ctx) = 
        eval (values, ctx)
        ==> function
            | (Set items, ctx)
            | (Q (Ket items), ctx) -> 
                (Ket items, ctx) |> Ok
            | (v, _) -> 
                $"Invalid value for a Ket element: {v}" |> Error

    and eval = (evalCore<QuantumExpression, QuantumValue> evalQuantum)
