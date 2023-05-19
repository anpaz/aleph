module CoinFlip

open aleph.kets
open context

let program() =
    let coin = ket 1
    sample [coin]
