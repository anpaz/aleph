module CoinFlip

open aleph.parser.ast

let program =
    Block(
        [
          // let coin = | 1, 0 >
          Let("coin", Ket(Set [ Int 1; Int 0 ])) ],
        // | coin |
        Sample(Prepare(Var "coin"))
    )
