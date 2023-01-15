module SolveEquation

open aleph.parser.ast

let program =
    Block(
        [
          // let x = | @, 3 >
          Let("x", KetAll(Int 3))

          // let eq1 = x + 3
          Let("eq1", Add(Var "x", Int 3))
          // let eq2 = 2 * x
          Let("eq2", Multiply(Int 2, Var "x"))

          // let solution = (x, eq1, eq2) | eq1 == eq2 : 1
          Let("solution", Filter((Join(Var "x", Join(Var "eq1", Var "eq2")), Equals(Var "eq1", Var "eq2")))) ],

        // |`solution`|
        Sample(Prepare(Var "solution"))
    )
