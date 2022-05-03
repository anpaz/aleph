module GraphColoring

open aleph.parser.core
open aleph.parser.quantum


let program = Block ([
    // let RED   = 0
    // let BLUE  = 1
    // let GREEN = 2
    Let ("RED",   Int 0);
    Let ("BLUE",  Int 1);
    Let ("GREEN", Int 2);


    // // Return the list of all available colors:
    // let colors() =
    //     [ RED, BLUE, GREEN ]
    Let ("colors", Method (
        arguments = List.empty, 
        body = Set [Id "RED"; Id "BLUE"; Id"GREEN"]));

    // // Edges are listed classically, so we can iterate through them
    // let edges = [
    //   (0, 1),
    //   (1, 2),
    //   (3, 1), 
    //   (2, 0)
    // ]
    Let ("edges", Set [
        Tuple [Int 0; Int 1]
        Tuple [Int 1; Int 2]
        Tuple [Int 3; Int 1]
        Tuple [Int 2; Int 0]
    ])
    Print ("edges", [Id "edges"])

    // // checks if the coloring for the nodes x and y is invalid.
    // // invalid is when the 2 nodes of an edge have the same color.
    // let is_valid_edge_coloring | color1 color2 =
    //     color1 != color2
    //
    Let ("is_valid_edge_coloring", Q (Unitary (
        arguments = [ "edge" ],
        qegs= [ "nodes_color" ],
        body= (Not (Equals (Id "color1", Id "color2"))))))

    // // A valid color combination oracle.
    // // Returns true only if the nodes' color combination is valid for all edges.
    // let classify_coloring edges | coloring =
    //     let valid = summarize e in edges with and
    //         let (x, y) = e
    //         is_valid_edge_coloring | coloring[x, y]
    //     valid
    //
    Let ("classify_coloring", Q (Unitary (
        arguments=[ "edges" ],
        qegs=[ "coloring" ],
        body = 
            Summarize ("e", Id "edges", "and", Block ([
                Let ("x", Project (Id "e", [Int 0]))
                Let ("y", Project (Id "e", [Int 1]))],
                Q (CallUnitary (
                    id="is_invalid_edge_coloring", 
                    arguments=[Id "e"],
                    ket = Project (Id "coloring", [Id "x"; Id "y"]))))))))

    // // A ket with the color combination for all nodes. Each node is an item of a tuple.
    // let nodes_colors = | (colors(), colors(), colors(), colors()) >
    Let("nodes_colors", (Q (Ket [
        Tuple [
            CallMethod ("colors", List.empty)
            CallMethod ("colors", List.empty)
            CallMethod ("colors", List.empty)
            CallMethod ("colors", List.empty)
        ]
    ])))

    Print ("nodes_colors", [Id "nodes_colors"])

    // // To find a valid coloring, solve the valid_combination oracle and
    // // measure the result
    // let all = classify_combinations (edges) nodes_colors
    Let("all", Q (CallUnitary (
        id="classify_combinations", 
        arguments=[Id "edges"],
        ket= Id "nodes_colors")))

    // let answers = solve(all)
    Let ("answers", Q (Solve (Id "all")))

    Print ("all", [Id "all"])
    Print ("answers", [Id "answers"])

    // let s1 = | answers |
    // let s2 = | answers |
    // let s2 = | answers |
    Let("s1", Q (Measure (Id "answers")))
    Let("s2", Q (Measure (Id "answers")))
    Let("s3", Q (Measure (Id "answers")))

    Print ("s1", [Id "s1"])
    Print ("s2", [Id "s2"])
    Print ("s3", [Id "s3"])

    ],

    // s3
    Id("s3"))

