module DiceRoll

open aleph.kets

let program : Ket list * Option<Ket> =
    let dice1 = Ket(Literal (width=3)).Where(In [1..6])
    let dice2 = Ket(Literal (width=3)).Where(In [1..6])

    let roll = dice1.Add(dice2)
    [ dice1; dice2; roll ], None
