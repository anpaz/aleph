module RandomNumber

open aleph.kets
open aleph.qpu.classic.context

let program() =
    let random = ket 10
    sample [random]
