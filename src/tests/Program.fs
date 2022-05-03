module Program 

open aleph.parser.core
open aleph.runtime.Core

[<EntryPoint>]
let main _ =
    // let printValue errors e = 
    //     let v = eval(e, Map.empty)

    //     match v with
    //     | Result.Ok (v, ctx) -> 
    //         printfn $"Value: {v}"
    //         errors + 0
    //     | Result.Error msg ->
    //         printfn "Error: %s"  msg
    //         errors + 1


    // // let k1 = 
    // //     ast.Ket([
    // //         ast.Set([
    // //             ast.Tuple([ast.Int(0); ast.Int(0)])
    // //             ast.Tuple([ast.Int(1); ast.Int(1)])
    // //         ])
    // //         ast.Tuple([ast.Int(0); ast.Int(1)])
    // //         ast.Tuple([ast.Int(1); ast.Int(1)])])
    // // let k2 = ast.Ket ([1..10] |> List.map ast.Int)    

    // [
    //     ast.Int(3)
    //     ast.Tuple([ast.Int(1); ast.Int(2); ast.Int(1)])
    //     ast.Set([ast.Int(1); ast.Int(2); ast.Int(1)])
    //     ast.Set([ast.Tuple([ast.Int(1); ast.Int(2); ast.Int(1)])])
    //     ast.Tuple([
    //         ast.Tuple([ast.Int(0); ast.Int(1)])
    //         ast.Int(2)
    //         ast.Tuple([
    //             ast.Tuple([ast.Int(3); ast.Int(4)])
    //             ast.Int(5)
    //         ])])
    //     // k1
    //     // k2
    // ]
    // |> List.fold printValue 0
    // |> ignore

    // // printfn "---- Measure k1:"
    // // for _ in 1..10 do
    // //     printValue 0 (ast.Measure k1) |> ignore

    // // printfn "---- Measure k2:"
    // // for _ in 1..10 do
    // //     printValue 0 (ast.Measure k2) |> ignore
    0

