open aleph.runtime.Classic

[<EntryPoint>]
let main argv =

    let result = (run Map.empty GraphColoring.program)

    match result with
    | Continue -> printfn "Missing return statement"
    | Result v ->  printfn $"result: {v}"
    | Error msg -> printfn $"!! Failed: {msg} !!"

    Utils.wrapup