module Sandbox

open aleph.kets
open context

let program () =
    let a = ket 2
    let b = ket 2
    let results = a.Equals(b)

    [ a; b; results ], None
