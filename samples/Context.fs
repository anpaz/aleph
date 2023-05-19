module context

open aleph.kets
open aleph.qpu.classic.context
open aleph.qpu.qsharp.context

let sample (kets: Ket list) =
    sample kets

let sample_when (kets: Ket list, filter: Ket) =
    sample_when (kets, filter)