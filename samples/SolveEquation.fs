module SolveEquation

open aleph.kets
open aleph.qpu.classic.context

let program() =
    let x = ket 3
    let eq1 = x.Add(3)
    let eq2 = x.Multiply(2)
    
    sample_when ([x; eq1; eq2], eq1.Equals(eq2))
