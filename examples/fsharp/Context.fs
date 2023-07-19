module context

open aleph.kets
open aleph.qpu.classic.context
// Change to qsharp context, by uncommenting this line:
//open aleph.qpu.qsharp.context

let sample (kets: KetValue list) = sample kets

let sample_when (kets: KetValue list) (filter: KetValue) = sample_when kets filter

let histogram (kets: KetValue list) (rounds: int) = histogram kets rounds
