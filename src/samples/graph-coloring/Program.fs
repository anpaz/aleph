open aleph.compiler.ast

let wrapup =
    printfn "=. aleph .="
    0

let RED   = (Int 0)
let BLUE  = (Int 1)
let GREEN = (Int 2)

[<EntryPoint>]
let main argv =
    
    //let a = (Set (Set {RED, BLUE, GREEN}))

    printfn $"RED: {RED}"


    wrapup 