module SolveEquation

open aleph.parser.ast

let program = Block ([
    // let x = | @, 4 >
    // let y = | @, 4 >
    Let ("x", KetAll (Int 4))
    Let ("y", KetAll (Int 4))
    
    // let eq1 = 4 * x + 5 * y
    Let ("eq1", Add(Multiply(Int 4, Var "x"), Multiply(Int 5, Var "y")))
    // let eq2 = -6 * x + 20 * y
    Let ("eq2", (Add (Multiply (Int -6, Var "x"), (Multiply (Int 20, Var "y")))))

    // let solution = Solve ((x, y), eq1 == eq2)
    Let ("solution", Solve ((Join (Var "x", Var "y")), Equals(Var "eq1", Var "eq2")))
],
    // | Prepare (solution) |
    Sample (Prepare (Var "solution")))
