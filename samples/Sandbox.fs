module Sandbox

open aleph.kets
open aleph.qpu.classic.context

let program : Ket list * Option<Ket> =
  let a = Ket(Literal (width=2))
  let b = Ket(Literal (width=2))
  let results = a.Equals(b)

  [a; b; results], None