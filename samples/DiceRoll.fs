module DiceRoll

open aleph.parser.ast

let program =
    Block(
        [
          // let dice1 = | 1..6 >
          // let dice2 = | 1..6 >
          Let("dice1", Ket(Range(Int 1, Int 6)))
          Let("dice2", Ket(Range(Int 1, Int 6)))

          // let roll = dice1 + dice2
          Let("roll", Add(Var "dice1", Var "dice2")) ],

        // | coin |
        Sample(Var "roll")
    )
