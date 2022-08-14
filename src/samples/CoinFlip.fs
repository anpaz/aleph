module CoinFlip

open aleph.parser.ast

let program = Block ([
    // let coin = | 1, 0 >
    Let ("coin", Ket [Int 1; Int 0])
],
    // | Prepare coin |
    Sample (Prepare (Var "coin")))
