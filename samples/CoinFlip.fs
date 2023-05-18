module CoinFlip

open aleph.kets

let program : Ket list * Option<Ket> =
  let coin = Ket(Literal (width=1))
  [coin], None
