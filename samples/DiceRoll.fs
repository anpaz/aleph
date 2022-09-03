module DiceRoll

open aleph.parser.ast

let program = Block ([
    // let dice1 = |1,2,3,4,5,6> // todo: | 1..6 >
    // let dice2 = |1,2,3,4,5,6> // todo: | 1..6 >
    Let ("dice1", Ket (Set [Int 1; Int 2; Int 3; Int 4; Int 5; Int 6]))
    Let ("dice2", Ket (Set [Int 1; Int 2; Int 3; Int 4; Int 5; Int 6]))

    // let roll = dice1 + dice2
    Let ("roll", Add (Var "dice1", Var "dice2"))
],
    // | Prepare coin |
    Sample (Prepare (Var "roll")))
