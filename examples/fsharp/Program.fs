open context

[<EntryPoint>]
let main argv =

    let programs =
        [ Examples.coin_flip
          Examples.random_number
          Examples.dice_roll
          Examples.solve_equation
          Examples.tiny_graph_coloring
          Examples.small_graph_coloring ]

    // let programs = [ Sandbox.program ]

    // Get the histgraom of the sum of two dices:
    Examples.dice_roll_histogram |> Utils.simulate |> ignore

    programs |> List.map Utils.simulate |> ignore
    Utils.wrapup 0
