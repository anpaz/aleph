//open aleph.runtime.Classic

open aleph.runtime.simulator

[<EntryPoint>]
let main argv =

    let programs = 
        [
            CoinFlip.program
            DiceRoll.program
            DiceRoll.program
            SolveEquation.program
            GraphColoring.program
        ]
    
    programs 
    |> List.map Utils.simulate
    |> List.sum
