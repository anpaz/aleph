module GraphColoring

open aleph.compiler.ast

let program = (Block [
    // let RED   = 0
    // let BLUE  = 1
    // let GREEN = 2
    Let ("RED",   Int(0));
    Let ("BLUE",  Int(1));
    Let ("GREEN", Int(2));


    // // Return the list of all available colors:
    // classic colors() ->
    //     [ RED, BLUE, GREEN ]
    DefClassic (
        id= "colors", 
        arguments=List.empty, 
        body=Return (Set([Id("RED"); Id("BLUE"); Id("GREEN")]))
    );

    // // Edges are listed classically, so we can iterate through them
    // let edges = [
    //   (0, 1),
    //   (1, 2),
    //   (3, 1), 
    //   (2, 0)
    // ]
    Let ("edges", Set([
        Tuple([Int(0); Int(1)]);
        Tuple([Int(1); Int(2)]);
        Tuple([Int(3); Int(1)]);
        Tuple([Int(2); Int(0)]);
    ]));
    Print ("edges", [Id "edges"])

    // // checks if the coloring for the nodes x and y is invalid.
    // // invalid is when the 2 nodes of an edge have the same color.
    // quantum is_invalid_edge_coloring | color1 color2 =>
    //     color1 == color2
    //
    DefQuantum (
        id= "is_invalid_edge_coloring", 
        arguments=["edge"],
        ket="nodes_color",
        body= Block [
            //Print ("edge, nodes_color", [Id "edge"; Id "nodes_color"])
            Let ("x", Project(Id("edge"), [Int(0)]));
            Let ("y", Project(Id("edge"), [Int(1)]));
            Let ("color1", Project(Id("nodes_color"), [Id("x")]));
            Let ("color2", Project(Id("nodes_color"), [Id("y")]));
            Return (Equals(Id("color1"), Id("color2")))
        ]
    );

    // // A valid color combination oracle.
    // // Returns true only if the nodes' color combination is valid for all edges.
    // quantum classify_combination edges | (nodes_color: Ket) =>
    //     for e in edges:
    //        if is_invalid_edge_coloring(e) nodes_color :
    //            return false
    //     return true
    //
    DefQuantum (
        id= "classify_combinations", 
        arguments=["edges"],
        ket="nodes_color",
        body= Block [
            For ("e", Id("edges"), Block [
                If (
                    cond=CallQuantum(
                        id="is_invalid_edge_coloring", 
                        arguments=[Id("e")],
                        ket=Id("nodes_color")),
                    t=Return(Bool(false)),
                    f=Skip
                )
            ]);
            Return(Bool(true))
        ]
    );

    // // A ket with the color combination for all nodes. Each node is an item of a tuple.
    // let nodes_colors = | (colors(), colors(), colors(), colors()) >
    //
    Let("nodes_colors", Ket [
        Tuple [
            CallClassic("colors", List.empty)
            CallClassic("colors", List.empty)
            CallClassic("colors", List.empty)
            CallClassic("colors", List.empty)
        ]
    ]);

    Print ("nodes_colors", [Id "nodes_colors"])

    // // To find a valid coloring, solve the valid_combination oracle and
    // // measure the result
    // let all = classify_combinations (edges) nodes_colors
    Let("all", CallQuantum(
        id="classify_combinations", 
        arguments=[Id("edges")],
        ket=Id("nodes_colors")
    ));

    // let answers = solve(all)
    Let ("answers", Solve (Id "all"))

    Print ("all", [Id "all"])
    Print ("answers", [Id "answers"])

    // let s1 = | answers |
    // let s2 = | solve(a) |
    // let s2 = | solve(a) |
    Let("s1", Measure (Id"answers"));
    Let("s2", Measure (Id"answers"));
    Let("s3", Measure (Id"answers"));

    Print ("s1", [Id "s1"])
    Print ("s2", [Id "s2"])
    Print ("s3", [Id "s3"])

    // s3
    Return(Id("s3"))
])
