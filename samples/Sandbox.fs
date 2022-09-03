module Sandbox

open aleph.parser.ast

let program = Block ([
    Let ("a", Ket (Set [Bool false; Bool true]))
    Let ("b", Ket (Set [Bool false; Bool true]))
    Let ("result", Not (Or (Var "a", Var "b")))

    // Let ("a", KetAll (Int 2))
    // Let ("b", KetAll (Int 2))
    // Let ("result", Equals(Var "a", Var "b"))
],
    // | Prepare coin |
    Sample (Prepare (Join(Join(Var "a", Var "b"), Var "result"))))
