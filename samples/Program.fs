[<EntryPoint>]
let main argv =

    let programs =
        [ CoinFlip.program
          DiceRoll.program
          SolveEquation.program
          TinyGraphColoring.program 
          GraphColoring.program 
        ]

    //let programs = [ Sandbox.program ]

    programs |> List.map Utils.qsharp |> List.sum |> Utils.wrapup
