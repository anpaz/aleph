module GraphColoring

open aleph.parser.ast


let program =
    Block(
        [
          // let RED   = 0
          // let BLUE  = 1
          // let GREEN = 2
          Let("RED", Int 0)
          Let("BLUE", Int 1)
          Let("GREEN", Int 2)


          // // Return the list of all available colors:
          // let colors() Ket<Int> =
          //     | RED, BLUE, GREEN >
          Let(
              "colors",
              Method(
                  arguments = List.empty,
                  returns = Type.Ket [ Type.Int ],
                  body = Ket(Set [ Var "RED"; Var "BLUE"; Var "GREEN" ])
              )
          )

          // // Edges are listed classically, so we can iterate through them
          // let edges = {
          //   (0, 1);
          //   (1, 2);
          //   (3, 1);
          //   (2, 0)
          // }
          Let(
              "edges",
              Set
                  [ Tuple [ Int 0; Int 1 ]
                    Tuple [ Int 1; Int 2 ]
                    Tuple [ Int 3; Int 1 ]
                    Tuple [ Int 2; Int 0 ] ]
          )
          //Print("edges", [ Var "edges" ])

          // // checks if the coloring for the nodes x and y is invalid.
          // // invalid is when the 2 nodes of an edge have the same color.
          // let is_valid_edge_coloring (color1: Ket<Int>, color2: Ket<Int>) : Ket<Bool> =
          //     color1 != color2
          //
          Let(
              "is_valid_edge_coloring",
              (Method(
                  arguments = [ ("color1", Type.Ket [ Type.Int ]); ("color2", Type.Ket [ Type.Int ]) ],
                  returns = Type.Ket [ Type.Bool ],
                  body = (Not(Equals(Var "color1", Var "color2")))
              ))
          )

          // // A valid color combination oracle.
          // // Returns true only if the nodes' color combination is valid for all edges.
          // let classify_coloring (edges: Set<Tuple<Int, Int>>, coloring: Ket<Int, Int, Int>) : Ket<Bool> =
          //     if Count(edges) == 0 then
          //          | true >
          //     else
          //         let e = Element(edges)
          //         let rest = Remove(e, edges)
          //         let x = e[0]
          //         let y = e[1]
          //         let one = is_valid_edge_coloring (coloring[x], coloring[y])
          //         if Count(rest) == 0 then
          //              one
          //         else
          //             one && classify_coloring(rest, coloring)
          //         valid
          //
          Let(
              "classify_coloring",
              (Method(
                  arguments =
                      [ ("edges", Type.Set(Type.Tuple [ Type.Int; Type.Int ]))
                        ("coloring", Type.Ket [ Type.Int; Type.Int; Type.Int; Type.Int ]) ],
                  returns = Type.Ket [ Type.Bool ],
                  body =
                      If(
                          Equals(Count(Var "edges"), Int 0),
                          Ket(Bool true),
                          Block(
                              [ Let("e", Element(Var "edges"))
                                Let("rest", Remove(Var "e", Var "edges"))
                                Let("x", Project(Var "e", Int 0))
                                Let("y", Project(Var "e", Int 1))
                                Let(
                                    "one",
                                    CallMethod(
                                        method = Var "is_valid_edge_coloring",
                                        arguments =
                                            [ Project(Var "coloring", Var "x"); Project(Var "coloring", Var "y") ]
                                    )
                                ) ],
                              If(
                                  Equals(Count(Var "rest"), Int 0),
                                  Var("one"),
                                  And(
                                      Var "one",
                                      CallMethod(
                                          method = Var "classify_coloring",
                                          arguments = [ Var "rest"; Var "coloring" ]
                                      )
                                  )
                              )
                          )
                      )
              ))
          )

          // // A ket with the color combination for all nodes. Each node is an item of a tuple.
          // let all_colorings = (((colors(), colors()), colors()), colors())
          Let(
              "all_colorings",
              (Join(
                  Join(
                      Join(CallMethod(Var "colors", List.empty), CallMethod(Var "colors", List.empty)),
                      CallMethod(Var "colors", List.empty)
                  ),
                  CallMethod(Var "colors", List.empty)
              ))
          )

          // // To find a valid coloring, solve the valid_combination oracle and
          // // measure the result
          // let is_valid = classify_coloring (edges, all_colorings)
          Let(
              "is_valid",
              (CallMethod(method = Var "classify_coloring", arguments = [ Var "edges"; Var "all_colorings" ]))
          )

          // Filter out only those colorings that are valid:
          // let answers = all_colorings ~ is_valid : 2
          Let("answers", Filter(Var "all_colorings", Var "is_valid", Int 2)) ],

        // | answers |
        Sample(Var "answers")
    )
