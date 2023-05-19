module DiceRoll

open aleph.kets
open context

let program() =
    let dice1 = (ket 3).Where(In [1..6])
    let dice2 = (ket 3).Where(In [1..6])

    let roll = dice1.Add(dice2, width=4)
    sample [ dice1; dice2; roll ]
