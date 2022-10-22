[<EntryPoint>]
let main argv =

    let programs =
        [ //Sandbox.program
          CoinFlip.program
          DiceRoll.program
          //SolveEquation.program
          TinyGraphColoring.program 
          GraphColoring.program 
        ]

    programs |> List.map Utils.qsharp |> List.sum |> Utils.wrapup
