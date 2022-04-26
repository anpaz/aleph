module Program 

open aleph.compiler
open aleph.runtime.Classic

[<EntryPoint>]
let main _ =
    let printValue acc e = 
        let v = eval(e, Map.empty)

        match v with
        | Result.Ok (v, ctx) -> 
            printfn $"Value: {v}"
            acc + 0
        | Result.Error msg ->
            printfn "Error: %s"  msg
            acc + 1

    [
        ast.Int(3)
        ast.Tuple([ast.Int(1); ast.Int(2); ast.Int(1)])
        ast.Set([ast.Int(1); ast.Int(2); ast.Int(1)])
        ast.Set([ast.Tuple([ast.Int(1); ast.Int(2); ast.Int(1)])])
        ast.Set([
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
