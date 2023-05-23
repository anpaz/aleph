[<EntryPoint>]
let main argv =

    let programs =
        [ CoinFlip.program
          RandomNumber.program
          DiceRoll.program
          SolveEquation.program
          TinyGraphColoring.program
          GraphColoring.program ]

    // let programs = [ Sandbox.program ]

    programs |> List.map Utils.simulate |> ignore
    Utils.wrapup 0
