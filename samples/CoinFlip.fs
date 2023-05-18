module CoinFlip

open aleph.kets
open aleph.qpu.classic.context

let program() =
    let coin = Ket(Literal (width=1))
    sample [coin]
