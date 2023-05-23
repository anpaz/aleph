module context

open aleph.kets
open aleph.qpu.classic.context
open aleph.qpu.qsharp.context

let sample (kets: KetValue list) =
    sample kets

let sample_when (kets: KetValue list, filter: KetValue) =
    sample_when (kets, filter)