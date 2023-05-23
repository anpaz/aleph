module context

open aleph.utils
open aleph.kets
open aleph.qpu.qsharp.context
open aleph.qpu.classic.context

let sample (kets: KetValue list) =
    sample kets

let sample_when (kets: KetValue list, filter: KetValue) =
    sample_when (kets, filter)

let histogram (kets: KetValue list) =
    prepare kets
    ==> fun u -> u.Histogram(kets)