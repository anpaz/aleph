module Program 

open aleph.compiler
open aleph.runtime.Classic



[<EntryPoint>]
let main _ =

    let e = ast.Set([ast.Int(1); ast.Int(2)])
    let v = eval(e, Map.empty)

    printfn ( $"Value: {v}")
    0
