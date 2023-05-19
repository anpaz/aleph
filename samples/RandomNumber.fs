module RandomNumber

open aleph.kets
open context

let program() =
    let random = ket 10
    sample [random]
