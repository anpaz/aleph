//open aleph.runtime.Classic

open aleph.parser.typed

[<EntryPoint>]
let main argv =
    let t0 = IntLiteral (0, Type.Int)
    let t1 = IntLiteral (1, Type.Int)
    let s = C.Set ([t0; t1], Type.Set (Type.Tuple [Type.Int]))
    let k = Q.Constant (s, QType.Ket [QInt])

    let program = C.Block (
        [
            Let ("x", (Quantum k))
        ],
        C.Sample (Q.Var ("x", QType.Ket [QInt]), Type.Int),
        Type.Int
    )
    
    // let result = eval (GraphColoring.program, Map.empty)

    // match result with
    // | Ok (v, _) ->  printfn $"result: {v}"
    // | Error msg -> printfn $"\n!! Failed: {msg} !!"

    Utils.wrapup