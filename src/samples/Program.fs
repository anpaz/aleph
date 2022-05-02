open aleph.runtime.Classic

[<EntryPoint>]
let main argv =
    let result = eval (GraphColoring.program, Map.empty)

    match result with
    | Ok (v, _) ->  printfn $"result: {v}"
    | Error msg -> printfn $"\n!! Failed: {msg} !!"

    Utils.wrapup