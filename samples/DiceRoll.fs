module DiceRoll

open aleph.kets
open aleph.qpu.classic.context

let program() =
    let dice1 = Ket(Literal (width=3)).Where(In [1..6])
    let dice2 = Ket(Literal (width=3)).Where(In [1..6])

    let roll = dice1.Add(dice2, width=4)
    sample [ dice1; dice2; roll ]
