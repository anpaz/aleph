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
    // classic is_invalid_edge_coloring(edge, nodes_color) =>
    //     let x = edge.0
    //     let y = edge.1
    //     let color1 = nodes_color.x
    //     let color2 = nodes_color.y
    //     color1 == color2
    //
    DefClassic (
        id= "is_invalid_edge_coloring", 
        arguments=["edge"; "nodes_color"],
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
    // quantum classify_combination(edges) nodes_color =>
    //     for e in edges:
    //        if is_invalid_edge_coloring(e) nodes_color :
    //            false
    //     true
    DefQuantum (
        id= "classify_combinations", 
        arguments=["edges"],
        ket="nodes_color",
        body= Block [
            For ("e", Id("edges"), Block [
                If (
                    cond=CallClassic(
                        id="is_invalid_edge_coloring", 
                        arguments=[Id("e"); Id("nodes_color")]),
                    t=Return(Bool(false)),
                    f=Skip
                )
            ]);
            Return(Bool(true))
        ]
    );

    // // A ket with the color combination for all nodes. Each node is an item of a tuple.
    // let nodes_colors = | colors(), colors(), colors(), colors() >
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
    // let a = classify_combinations (edges) nodes_colors
    Let("a", CallQuantum(
        id="classify_combinations", 
        arguments=[Id("edges")],
        ket=Id("nodes_colors")
    ));

    Print ("a", [Id "a"])
    Print ("solve", [Solve (Id "a")])

    // let s1 = | solve(a) |
    Let("s1", Measure((Solve(Id("a")))));
    Print ("s1", [Id "s1"])

    // let s2 = | solve(a) |
    Let("s2", Measure((Solve(Id("a")))));
    Print ("s2", [Id "s2"])

    // let s2 = | solve(a) |
    Let("s3", Measure((Solve(Id("a")))));
    Print ("s3", [Id "s3"])

    // solution
    Return(Id("s3"))
])
