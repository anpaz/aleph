module SolveEquation

open aleph.parser.ast

let program =
    Block(
        [
          // let x = | @, 3 >
          Let("x", KetAll(Int 3))

          // let eq1 = x + 5
          Let("eq1", Add(Var "x", Int 3))
          // let eq2 = 2x
          Let("eq2", Multiply(Int 2, Var "x"))

          // let solution = Filter ((x, y), eq1 == eq2)
          Let("solution", Filter(Var "x", Equals(Var "eq1", Var "eq2"), Int 1)) ],

        // | Prepare (solution) |
        Sample(Prepare(Var "solution"))
    )
