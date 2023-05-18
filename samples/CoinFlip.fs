module CoinFlip

open aleph.kets
open aleph.qpu.classic.context

let program() =
    let coin = ket 1
    sample [coin]
