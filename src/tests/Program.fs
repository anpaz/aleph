module Program 

open aleph.compiler
open aleph.runtime.Classic

[<EntryPoint>]
let main _ =
    let printValue errors e = 
        let v = eval(e, Map.empty)

        match v with
        | Result.Ok (v, ctx) -> 
            printfn $"Value: {v}"
            errors + 0
        | Result.Error msg ->
            printfn "Error: %s"  msg
            errors + 1

    [
        ast.Int(3)
        ast.Tuple([ast.Int(1); ast.Int(2); ast.Int(1)])
        ast.Set([ast.Int(1); ast.Int(2); ast.Int(1)])
        ast.Set([ast.Tuple([ast.Int(1); ast.Int(2); ast.Int(1)])])
        ast.Ket([
            ast.Set([
                ast.Tuple([ast.Int(0); ast.Int(0)])
                ast.Tuple([ast.Int(1); ast.Int(1)])
            ])
            ast.Tuple([ast.Int(0); ast.Int(1)])
            ast.Tuple([ast.Int(1); ast.Int(1)])])
        ast.Tuple([
            ast.Tuple([ast.Int(0); ast.Int(1)])
            ast.Int(2)
            ast.Tuple([
                ast.Tuple([ast.Int(3); ast.Int(4)])
                ast.Int(5)
            ])])
    ]
    |> List.fold printValue 0
    |> ignore

    printfn "----"
    let k = 
        ast.Ket([
            ast.Set([
                ast.Tuple([ast.Int(0); ast.Int(0)])
                ast.Tuple([ast.Int(1); ast.Int(1)])
            ])
            ast.Tuple([ast.Int(0); ast.Int(1)])
            ast.Tuple([ast.Int(1); ast.Int(1)])])
    for _ in 1..20 do
        printValue 0 (ast.Measure k) |> ignore

    printfn "----"
    let k = ast.Ket ([1..10] |> List.map ast.Int)
    for _ in 1..10 do
        printValue 0 (ast.Measure k) |> ignore
    0

