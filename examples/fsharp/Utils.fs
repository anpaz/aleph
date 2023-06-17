module Utils

open aleph.kets


let wrapup code =
    let version = typeof<KetValue>.Assembly.GetName().Version.ToString()
    printfn ""
    printfn "%s" aleph.utils.signature
    code

let simulate program =
    let result = program ()
    printfn "result: %A" result
