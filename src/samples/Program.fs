open aleph.runtime.Classic

[<EntryPoint>]
let main argv =
    let result = run (GraphColoring.program, Map.empty)

    match result with
    | Continue _ -> printfn "Missing return statement"
    | Result (v, _) ->  printfn $"result: {v}"
    | Error (msg, _) -> printfn $"\n!! Failed: {msg} !!"

    Utils.wrapup