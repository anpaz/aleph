[<EntryPoint>]
let main argv =

    let programs = 
        [
            //Sandbox.program
            // CoinFlip.program
            DiceRoll.program
            // SolveEquation.program
            // GraphColoring.program
        ]
    
    programs 
    |> List.map Utils.trace
    |> List.sum
