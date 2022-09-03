module GraphColoring

open aleph.parser.ast


let program = Block ([
    // let RED   = 0
    // let BLUE  = 1
    // let GREEN = 2
    Let ("RED",   Int 0)
    Let ("BLUE",  Int 1)
    Let ("GREEN", Int 2)


    // // Return the list of all available colors:
    // let colors() =
    //     | RED, BLUE, GREEN >
    Let ("colors", Method (
        arguments = List.empty, 
        body = Ket (Set [Var "RED"; Var "BLUE"; Var"GREEN"])))

    // // Edges are listed classically, so we can iterate through them
    // let edges = {
    //   (0, 1),
    //   (1, 2),
    //   (3, 1), 
    //   (2, 0)
    // }
    Let ("edges", Set [
        Tuple [Int 0; Int 1]
        Tuple [Int 1; Int 2]
        Tuple [Int 3; Int 1]
        Tuple [Int 2; Int 0]
    ])
    Print ("edges", [Var "edges"])

    // // checks if the coloring for the nodes x and y is invalid.
    // // invalid is when the 2 nodes of an edge have the same color.
    // let is_valid_edge_coloring (color1: Ket<Int>, color2: Ket<Int>) =
    //     color1 != color2
    //
    Let ("is_valid_edge_coloring", (Method (
        arguments = [ 
            ("color1", QType (QType.Ket [Type.Int]))
            ("color2", QType (QType.Ket [Type.Int]))
        ],
        body = (Not (Equals (Var "color1", Var "color2"))))))

    // // A valid color combination oracle.
    // // Returns true only if the nodes' color combination is valid for all edges.
    // let classify_coloring (edges: Set<Tuple<Int, Int>>, coloring: Ket<Int, Int, Int>)  =
    //     let valid = summarize e in edges with and
    //         let x = e[0]
    //         let y = e[1]
    //         is_valid_edge_coloring (coloring[x], coloring[y])
    //     valid
    //
    Let ("classify_coloring", (Method (
        arguments=[ 
            ("edges", Type (Type.Set (Type.Tuple [Type.Int; Type.Int])))
            ("coloring", QType (QType.Ket [Type.Int; Type.Int; Type.Int; Type.Int]))
        ],
        body = 
            Summarize ("e", Var "edges", Aggregation.And, Block ([
                Let ("x", Project (Var "e", Int 0))
                Let ("y", Project (Var "e", Int 1))],
                CallMethod (
                    method = Var "is_valid_edge_coloring", 
                    arguments  = [
                        Project (Var "coloring", Var "x")
                        Project (Var "coloring", Var "y")
                    ]))))))

    // // A ket with the color combination for all nodes. Each node is an item of a tuple.
    // let nodes_colors = (((colors(), colors()), colors()), colors())
    Let("nodes_colors", (Join (Join (Join (
            CallMethod (Var "colors", List.empty),
            CallMethod (Var "colors", List.empty)),
            CallMethod (Var "colors", List.empty)),
            CallMethod (Var "colors", List.empty))))

    Print ("nodes_colors", [Var "nodes_colors"])

    // // To find a valid coloring, solve the valid_combination oracle and
    // // measure the result
    // let all = classify_coloring (edges, nodes_colors)
    Let("classification", (CallMethod (
        method = Var "classify_coloring", 
        arguments = [
            Var "edges"
            Var "nodes_colors"
        ])))

    // let answers = Solve(nodes_colors, classification)
    Let ("answers", Solve (Var "nodes_colors", Var "classification"))

    Print ("classification", [Var "classification"])
    Print ("answers", [Var "answers"])

    // let s1 = | Prepare (answers) |
    // let s2 = | Prepare (answers) |
    // let s2 = | Prepare (answers) |
    Let ("s1", Sample (Prepare (Var "answers")))
    Let ("s2", Sample (Prepare (Var "answers")))
    Let ("s3", Sample (Prepare (Var "answers")))

    Print ("s1", [Var "s1"])
    Print ("s2", [Var "s2"])
    Print ("s3", [Var "s3"])
],
    // s3
    Var("s3"))

