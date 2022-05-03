namespace aleph.parser.quantum

open aleph.parser.core

type QuantumExpression =
    | Ket of values: Expression<QuantumExpression> list
    | Unitary of arguments: string list * qegs: string list * body: Expression<QuantumExpression>
    | CallUnitary of id: string * arguments: Expression<QuantumExpression> list * ket: Expression<QuantumExpression>
    | Measure of ket: Expression<QuantumExpression>
    | Solve of ket: Expression<QuantumExpression>
    | All

