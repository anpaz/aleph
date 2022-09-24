namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.parser.ast
open aleph.parser.TypeChecker
open aleph.runtime.Eval

open aleph.tests.Utils

[<TestClass>]
(*
    These test take an untyped quantum (ket) expression, and
    prepares the classical processor with the resulting Ket; they
    then verify that the quantum state and the returned columns
    from the preparation matches some expected values.
*)
type TestQPUClassic() =
    member this.Context =
        { ClassicValueContext.ctx with qpu = aleph.runtime.qpu.classic.Processor() }

    [<TestMethod>]
    member this.TestBasicExpressions() =
        let ctx = this.Context

        [
          // | false >
          e.Ket(e.Bool false), [ [ Bool false ] ], [ 0 ]
          // | 0, 1, 2 >
          e.Ket(e.Set [ e.Int 0; e.Int 1; e.Int 2 ]), [ [ Int 0 ]; [ Int 1 ]; [ Int 2 ] ], [ 0 ]
          // | (0,0), (0,1), (1,1) >
          e.Ket(
              e.Set
                  [ e.Tuple [ e.Int 0; e.Int 0 ]
                    e.Tuple [ e.Int 0; e.Int 1 ]
                    e.Tuple [ e.Int 1; e.Int 1 ] ]
          ),
          [ [ Int 0; Int 0 ]; [ Int 0; Int 1 ]; [ Int 1; Int 1 ] ],
          [ 0; 1 ]
          // | (0,0,0), (0,1,1), (1,1,0), (1,1,2) >.2
          e.Project(
              e.Ket(
                  e.Set
                      [ e.Tuple [ e.Int 0; e.Int 0; e.Int 0 ]
                        e.Tuple [ e.Int 0; e.Int 1; e.Int 1 ]
                        e.Tuple [ e.Int 1; e.Int 1; e.Int 0 ]
                        e.Tuple [ e.Int 1; e.Int 1; e.Int 2 ] ]
              ),
              e.Int 2
          ),
          [ [ Int 0; Int 0; Int 0 ]
            [ Int 0; Int 1; Int 1 ]
            [ Int 1; Int 1; Int 0 ]
            [ Int 1; Int 1; Int 2 ] ],
          [ 2 ]
          // ( | 0, 1 >, | 1, 2 > )
          e.Join(e.Ket(e.Set [ e.Int 0; e.Int 1 ]), e.Ket(e.Set [ e.Int 1; e.Int 2 ])),
          [ [ Int 0; Int 1 ]; [ Int 0; Int 2 ]; [ Int 1; Int 1 ]; [ Int 1; Int 2 ] ],
          [ 0; 1 ]
          e.KetAll(e.Int 3),
          [ [ Int 0 ]
            [ Int 1 ]
            [ Int 2 ]
            [ Int 3 ]
            [ Int 4 ]
            [ Int 5 ]
            [ Int 6 ]
            [ Int 7 ] ],
          [ 0 ]
          e.Join(e.Ket(e.Set [ e.Tuple [ e.Int 0; e.Int -6 ] ]), e.KetAll(e.Int 2)),
          [ [ Int 0; Int -6; Int 0 ]
            [ Int 0; Int -6; Int 1 ]
            [ Int 0; Int -6; Int 2 ]
            [ Int 0; Int -6; Int 3 ] ],
          [ 0; 1; 2 ] ]
        |> List.iter (this.TestExpression ctx)


    [<TestMethod>]
    member this.TestAddMultiply() =
        let ctx =
            this.Context
            |> add_to_context
                "k1"
                (Type.Ket [ Type.Int; Type.Int ])
                (e.Ket(
                    e.Set
                        [ e.Tuple [ e.Int 0; e.Int 0 ]
                          e.Tuple [ e.Int 0; e.Int 1 ]
                          e.Tuple [ e.Int 1; e.Int 1 ] ]
                ))

        [
          // k1.0 + k1.1
          e.Add(e.Project(e.Var "k1", e.Int 0), e.Project(e.Var "k1", e.Int 1)),
          [ [ Int 0; Int 0; Int 0 ]; [ Int 0; Int 1; Int 1 ]; [ Int 1; Int 1; Int 2 ] ],
          [ 2 ]
          // k1.0 + 1
          e.Add(e.Project(e.Var "k1", e.Int 0), e.Int 1),
          [ [ Int 0; Int 0; Int 1; Int 1 ]
            [ Int 0; Int 1; Int 1; Int 1 ]
            [ Int 1; Int 1; Int 1; Int 2 ] ],
          [ 3 ]
          // k1.0 * 5
          e.Multiply(e.Project(e.Var "k1", e.Int 0), e.Int 5),
          [ [ Int 0; Int 0; Int 5; Int 0 ]
            [ Int 0; Int 1; Int 5; Int 0 ]
            [ Int 1; Int 1; Int 5; Int 5 ] ],
          [ 3 ]
          // k1.0 + | 1, 2, 3 >
          e.Add(e.Project(e.Var "k1", e.Int 1), e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ])),
          [ [ Int 0; Int 0; Int 1; Int 1 ]
            [ Int 0; Int 0; Int 2; Int 2 ]
            [ Int 0; Int 0; Int 3; Int 3 ]
            [ Int 0; Int 1; Int 1; Int 2 ]
            [ Int 0; Int 1; Int 2; Int 3 ]
            [ Int 0; Int 1; Int 3; Int 4 ]
            [ Int 1; Int 1; Int 1; Int 2 ]
            [ Int 1; Int 1; Int 2; Int 3 ]
            [ Int 1; Int 1; Int 3; Int 4 ] ],
          [ 3 ]
          // Join (k1.0, k1.1 + | 1, 2, 3 >)
          e.Join(
              e.Project(e.Var "k1", e.Int 0),
              e.Add(e.Project(e.Var "k1", e.Int 1), e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ]))
          ),
          [ [ Int 0; Int 0; Int 1; Int 1 ]
            [ Int 0; Int 0; Int 2; Int 2 ]
            [ Int 0; Int 0; Int 3; Int 3 ]
            [ Int 0; Int 1; Int 1; Int 2 ]
            [ Int 0; Int 1; Int 2; Int 3 ]
            [ Int 0; Int 1; Int 3; Int 4 ]
            [ Int 1; Int 1; Int 1; Int 2 ]
            [ Int 1; Int 1; Int 2; Int 3 ]
            [ Int 1; Int 1; Int 3; Int 4 ] ],
          [ 0; 3 ]
          // Join (k1.0, k1.1 * | 1, 2, 3 >)
          e.Join(
              e.Project(e.Var "k1", e.Int 0),
              e.Multiply(e.Project(e.Var "k1", e.Int 1), e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ]))
          ),
          [ [ Int 0; Int 0; Int 1; Int 0 ]
            [ Int 0; Int 0; Int 2; Int 0 ]
            [ Int 0; Int 0; Int 3; Int 0 ]
            [ Int 0; Int 1; Int 1; Int 1 ]
            [ Int 0; Int 1; Int 2; Int 2 ]
            [ Int 0; Int 1; Int 3; Int 3 ]
            [ Int 1; Int 1; Int 1; Int 1 ]
            [ Int 1; Int 1; Int 2; Int 2 ]
            [ Int 1; Int 1; Int 3; Int 3 ] ],
          [ 0; 3 ] ]
        |> List.iter (this.TestExpression ctx)

    [<TestMethod>]
    member this.TestJoin() =
        let ctx =
            this.Context
            |> add_to_context
                "k1"
                (Type.Ket [ Type.Int; Type.Int ])
                (e.Ket(
                    e.Set
                        [ e.Tuple [ e.Int 0; e.Int 0 ]
                          e.Tuple [ e.Int 0; e.Int 1 ]
                          e.Tuple [ e.Int 1; e.Int 1 ] ]
                ))
            |> add_to_context "k2" (Type.Ket [ Type.Int ]) (e.Ket(e.Set [ e.Tuple [ e.Int 1 ]; e.Tuple [ e.Int 3 ] ]))

        [
          // let x = 10
          // let y = x
          // let x = false
          // (x, y)
          e.Block(
              [ s.Let("x", e.Ket(e.Int 10))
                s.Let("y", e.Var "x")
                s.Let("x", e.Ket(e.Bool false)) ],
              e.Join(e.Var "x", e.Var "y")
          ),
          [ [ Bool false; Int 10 ] ],
          [ 0; 1 ]

          // Join (k1, k1)
          e.Join(e.Var "k1", e.Var "k1"), [ [ Int 0; Int 0 ]; [ Int 0; Int 1 ]; [ Int 1; Int 1 ] ], [ 0; 1; 0; 1 ]
          // Join (k1, k2)
          e.Join(e.Var "k1", e.Var "k2"),
          [ [ Int 0; Int 0; Int 1 ]
            [ Int 0; Int 0; Int 3 ]
            [ Int 0; Int 1; Int 1 ]
            [ Int 0; Int 1; Int 3 ]
            [ Int 1; Int 1; Int 1 ]
            [ Int 1; Int 1; Int 3 ] ],
          [ 0; 1; 2 ]
          // (Join k1, |true, false>)
          e.Join(e.Var "k1", e.Ket(e.Set [ e.Bool true; e.Bool false ])),
          [ [ Int 0; Int 0; Bool false ]
            [ Int 0; Int 0; Bool true ]
            [ Int 0; Int 1; Bool false ]
            [ Int 0; Int 1; Bool true ]
            [ Int 1; Int 1; Bool false ]
            [ Int 1; Int 1; Bool true ] ],
          [ 0; 1; 2 ]
          // let e1 = k1.1 + 10
          // let e2 = k2 + 10
          // (Join k1, e2 == e1 )
          e.Block(
              [ Let("e1", e.Add(e.Project(e.Var "k1", e.Int 1), e.Int 10))
                Let("e2", e.Add(e.Var "k2", e.Int 10)) ],
              e.Join(e.Var "k1", e.Equals(e.Var "e2", e.Var "e1"))
          ),
          [ [ Int 0; Int 0; Int 1; Int 10; Int 11; Int 10; Int 10; Bool false ]
            [ Int 0; Int 0; Int 3; Int 10; Int 13; Int 10; Int 10; Bool false ]
            [ Int 0; Int 1; Int 1; Int 10; Int 11; Int 10; Int 11; Bool true ]
            [ Int 0; Int 1; Int 3; Int 10; Int 13; Int 10; Int 11; Bool false ]
            [ Int 1; Int 1; Int 1; Int 10; Int 11; Int 10; Int 11; Bool true ]
            [ Int 1; Int 1; Int 3; Int 10; Int 13; Int 10; Int 11; Bool false ] ],
          [ 0; 1; 7 ]


          // let alpha = |3,5>
          // let x =
          //    let alpha = alpha * 10
          //    let x =
          //      let alpha = alpha * 20
          //      alpha
          //    (alpha, x)
          // (alpha, x)
          e.Block(
              [ s.Let("alpha", e.Ket(e.Set [ e.Int 3; e.Int 5 ]))
                s.Let(
                    "x",
                    e.Block(
                        [ s.Let("alpha", e.Multiply(e.Var "alpha", e.Int 10))
                          s.Let("x", e.Block([ s.Let("alpha", e.Multiply(e.Var "alpha", e.Int 20)) ], e.Var "alpha")) ],
                        e.Join(e.Var "alpha", e.Var "x")
                    )
                ) ],
              e.Join(e.Var "alpha", e.Var "x")
          ),
          [ [ Int 3; Int 10; Int 30; Int 20; Int 600 ]
            [ Int 5; Int 10; Int 50; Int 20; Int 1000 ] ],
          [ 0; 2; 4 ]

          ]
        |> List.iter (this.TestExpression ctx)


    [<TestMethod>]
    member this.TestIndex() =
        let ctx =
            this.Context
            |> add_to_context
                "k1"
                (Type.Ket [ Type.Int; Type.Int ])
                (e.Ket(
                    e.Set
                        [ e.Tuple [ e.Int 0; e.Int 0 ]
                          e.Tuple [ e.Int 0; e.Int 1 ]
                          e.Tuple [ e.Int 1; e.Int 1 ] ]
                ))

        [
          // k1.0 + k1.[0 + 1]
          e.Add(e.Project(e.Var "k1", e.Int 0), e.Project(e.Var "k1", e.Add(e.Int 0, e.Int 1))),
          [ [ Int 0; Int 0; Int 0 ]; [ Int 0; Int 1; Int 1 ]; [ Int 1; Int 1; Int 2 ] ],
          [ 2 ]
          // k1.0 + k1.[0 + 3]        // Index is modular, so 3 == 1
          e.Add(e.Project(e.Var "k1", e.Int 0), e.Project(e.Var "k1", e.Add(e.Int 0, e.Int 3))),
          [ [ Int 0; Int 0; Int 0 ]; [ Int 0; Int 1; Int 1 ]; [ Int 1; Int 1; Int 2 ] ],
          [ 2 ] ]
        |> List.iter (this.TestExpression ctx)

    [<TestMethod>]
    member this.TestIfQ() =
        let ctx =
            this.Context
            |> add_to_context
                "k1"
                (Type.Ket [ Type.Int; Type.Int ])
                (e.Ket(
                    e.Set
                        [ e.Tuple [ e.Int 0; e.Int 0 ]
                          e.Tuple [ e.Int 0; e.Int 1 ]
                          e.Tuple [ e.Int 1; e.Int 1 ] ]
                ))
            |> add_to_context "k2" (Type.Ket [ Type.Int ]) (e.Ket(e.Set [ e.Tuple [ e.Int 2 ]; e.Tuple [ e.Int 3 ] ]))

        [
          // if k1.1 == k1.0 then k1.1 else 42
          e.If(
              e.Equals(e.Project(e.Var "k1", e.Int 1), e.Project(e.Var "k1", e.Int 0)),
              e.Project(e.Var "k1", e.Int 1),
              e.Int 42
          ),
          [ [ Int 0; Int 0; Bool true; Int 42; Int 0 ]
            [ Int 0; Int 1; Bool false; Int 42; Int 42 ]
            [ Int 1; Int 1; Bool true; Int 42; Int 1 ] ],
          [ 4 ] ]
        |> List.iter (this.TestExpression ctx)

    [<TestMethod>]
    member this.TestIfClassic() =
        let ctx =
            this.Context
            |> add_to_context
                "k1"
                (Type.Ket [ Type.Int; Type.Int ])
                (e.Ket(
                    e.Set
                        [ e.Tuple [ e.Int 0; e.Int 0 ]
                          e.Tuple [ e.Int 0; e.Int 1 ]
                          e.Tuple [ e.Int 1; e.Int 1 ] ]
                ))
            |> add_to_context "k2" (Type.Ket [ Type.Int ]) (e.Ket(e.Set [ e.Tuple [ e.Int 2 ]; e.Tuple [ e.Int 3 ] ]))

        [
          // if true then k1.1 else 42
          e.If(e.Bool true, e.Project(e.Var "k1", e.Int 1), e.Int 42),
          [ [ Int 0; Int 0 ]; [ Int 0; Int 1 ]; [ Int 1; Int 1 ] ],
          [ 1 ]
          // if false then k1.1 else 42
          e.If(e.Bool false, e.Project(e.Var "k1", e.Int 1), e.Int 42), [ [ Int 42 ] ], [ 0 ] ]
        |> List.iter (this.TestExpression ctx)


    [<TestMethod>]
    member this.TestSolveEquals() =
        let ctx =
            this.Context
            |> add_to_context
                "k1"
                (Type.Ket [ Type.Int; Type.Int ])
                (e.Ket(
                    e.Set
                        [ e.Tuple [ e.Int 0; e.Int 0 ]
                          e.Tuple [ e.Int 0; e.Int 1 ]
                          e.Tuple [ e.Int 1; e.Int 1 ] ]
                ))
            |> add_to_context "k2" (Type.Ket [ Type.Int ]) (e.Ket(e.Set [ e.Tuple [ e.Int 2 ]; e.Tuple [ e.Int 3 ] ]))

        [
          // k1.1 == 1
          e.Equals(e.Project(e.Var "k1", e.Int 1), e.Int 1),
          [ [ Int 0; Int 0; Int 1; Bool false ]
            [ Int 0; Int 1; Int 1; Bool true ]
            [ Int 1; Int 1; Int 1; Bool true ] ],
          [ 3 ]
          // (Solve k1, k1.1 == 1)
          e.Solve(e.Var "k1", e.Equals(e.Project(e.Var "k1", e.Int 1), e.Int 1)),
          [ [ Int 0; Int 1; Int 1; Bool true ]; [ Int 1; Int 1; Int 1; Bool true ] ],
          [ 0; 1 ]
          // (Solve k1, k1.0 + k1.1 == 1)
          e.Solve(e.Var "k1", e.Equals(e.Add(e.Project(e.Var "k1", e.Int 0), e.Project(e.Var "k1", e.Int 1)), e.Int 1)),
          [ [ Int 0; Int 1; Int 1; Int 1; Bool true ] ],
          [ 0; 1 ]
          // (Solve k1.0, k1.1 + | 1, 2, 3 > == |2, 4> )
          e.Solve(
              e.Project(e.Var "k1", e.Int 1),
              e.Equals(
                  e.Add(e.Project(e.Var "k1", e.Int 1), e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ])),
                  e.Ket(e.Set [ e.Int 2; e.Int 4 ])
              )
          ),
          [ [ Int 0; Int 0; Int 2; Int 2; Int 2; Bool true ]
            [ Int 0; Int 1; Int 1; Int 2; Int 2; Bool true ]
            [ Int 0; Int 1; Int 3; Int 4; Int 4; Bool true ]
            [ Int 1; Int 1; Int 1; Int 2; Int 2; Bool true ]
            [ Int 1; Int 1; Int 3; Int 4; Int 4; Bool true ] ],
          [ 1 ] ]
        |> List.iter (this.TestExpression ctx)


    [<TestMethod>]
    member this.TestBoolOps() =
        let ctx =
            this.Context
            |> add_to_context
                "k"
                (Type.Ket [ Type.Int; Type.Bool ])
                (e.Ket(
                    e.Set
                        [ e.Tuple [ e.Int 0; e.Bool true ]
                          e.Tuple [ e.Int 0; e.Bool false ]
                          e.Tuple [ e.Int 1; e.Bool true ] ]
                ))

        [ e.Var "k",
          [
            // Looks like because they are set, they are ordered differently from inputs:
            // this might be problematic for tests...
            [ Int 0; Bool false ]
            [ Int 0; Bool true ]
            [ Int 1; Bool true ] ],
          [ 0; 1 ]

          // not k.1
          e.Not(e.Project(e.Var "k", e.Int 1)),
          [ [ Int 0; Bool false; Bool true ]
            [ Int 0; Bool true; Bool false ]
            [ Int 1; Bool true; Bool false ] ],
          [ 2 ]

          // (false or k.1)
          e.Or(e.Bool false, e.Project(e.Var "k", e.Int 1)),
          [ [ Bool false; Int 0; Bool false; Bool false ]
            [ Bool false; Int 0; Bool true; Bool true ]
            [ Bool false; Int 1; Bool true; Bool true ] ],
          [ 3 ]

          // not (k.0 == 0 and k.1)
          e.Not(e.And(e.Equals(e.Project(e.Var "k", e.Int 0), e.Int 0), e.Project(e.Var "k", e.Int 1))),
          [ [ Int 0; Bool false; Int 0; Bool true; Bool false; Bool true ]
            [ Int 0; Bool true; Int 0; Bool true; Bool true; Bool false ]
            [ Int 1; Bool true; Int 0; Bool false; Bool false; Bool true ] ],
          [ 5 ] ]
        |> List.iter (this.TestExpression ctx)


    [<TestMethod>]
    member this.TestCallMethod() =
        let ctx = this.Context

        [
          // // let colors() = |1,2,3>
          // // colors()
          e.Block(
              [ Let(
                    "colors",
                    e.Method(
                        arguments = [],
                        returns = Type.Ket [ Type.Int ],
                        body = e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ])
                    )
                ) ],
              e.CallMethod(e.Var "colors", [])
          ),
          [ [ Int 1 ]; [ Int 2 ]; [ Int 3 ] ],
          [ 0 ]
          // let colors() = |1,2,3>
          // ( colors(), colors() )
          e.Block(
              [ Let(
                    "colors",
                    e.Method(
                        arguments = [],
                        returns = Type.Ket [ Type.Int ],
                        body = e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ])
                    )
                ) ],
              e.Join(e.CallMethod(e.Var "colors", []), e.CallMethod(e.Var "colors", []))
          ),
          [ [ Int 1; Int 1 ]
            [ Int 1; Int 2 ]
            [ Int 1; Int 3 ]
            [ Int 2; Int 1 ]
            [ Int 2; Int 2 ]
            [ Int 2; Int 3 ]
            [ Int 3; Int 1 ]
            [ Int 3; Int 2 ]
            [ Int 3; Int 3 ] ],
          [ 0; 1 ]
          // let k1 = |0,1>
          // let add_one(k: Ket<Int>) = k + 1
          // add_one(k1)
          e.Block(
              [ Let("k1", e.Ket(e.Set [ e.Int 0; e.Int 1 ]))
                Let(
                    "add_one",
                    e.Method(
                        arguments = [ "k", (Type.Ket [ Type.Int ]) ],
                        returns = Type.Ket [ Type.Int ],
                        body = e.Add(e.Var "k", e.Int 1)
                    )
                ) ],
              e.CallMethod(e.Var "add_one", [ e.Var "k1" ])
          ),
          [ [ Int 0; Int 1; Int 1 ]; [ Int 1; Int 1; Int 2 ] ],
          [ 2 ]
          // let k = |0,1>
          // let add_one(k: Ket<Int>) = k + 1
          // let k = add_one(k)
          // k
          e.Block(
              [ Let("k", e.Ket(e.Set [ e.Int 0; e.Int 1 ]))
                Let(
                    "add_one",
                    e.Method(
                        arguments = [ "k", (Type.Ket [ Type.Int ]) ],
                        returns = Type.Ket [ Type.Int ],
                        body = e.Add(e.Var "k", e.Int 1)
                    )
                )
                Let("k", e.CallMethod(e.Var "add_one", [ e.Var "k" ])) ],
              e.Var "k"
          ),
          [ [ Int 0; Int 1; Int 1 ]; [ Int 1; Int 1; Int 2 ] ],
          [ 2 ]
          // let k1 = |0,1>
          // let add_one(k: Ket<Int>) = k + 1
          // let k2 = add_one(k1)
          // let k1 = | 2,3 >
          // k2
          e.Block(
              [ Let("k1", e.Ket(e.Set [ e.Int 0; e.Int 1 ]))
                Let(
                    "add_one",
                    e.Method(
                        arguments = [ "k", (Type.Ket [ Type.Int ]) ],
                        returns = Type.Ket [ Type.Int ],
                        body = e.Add(e.Var "k", e.Int 1)
                    )
                )
                Let("k2", e.CallMethod(e.Var "add_one", [ e.Var "k1" ]))
                Let("k1", e.Ket(e.Set [ e.Int 2; e.Int 3 ])) ],
              e.Var "k2"
          ),
          [ [ Int 0; Int 1; Int 1 ]; [ Int 1; Int 1; Int 2 ] ],
          [ 2 ]
          // let prepare_bell(a: int, b: int) = Prepare( |(a,a), (b,b)> )
          // prepare_bell(2, i1)
          e.Block(
              [ Let(
                    "prepare_bell",
                    e.Method(
                        arguments = [ "a", Type.Int; "b", Type.Int ],
                        returns = Type.Universe [ Type.Int; Type.Int ],
                        body =
                            e.Prepare(
                                e.Ket(e.Set [ e.Tuple [ e.Var "a"; e.Var "a" ]; e.Tuple [ e.Var "b"; e.Var "b" ] ])
                            )
                    )
                ) ],
              e.CallMethod(e.Var "prepare_bell", [ e.Int 2; e.Var "i1" ])
          ),
          [ [ Int 1; Int 1 ]; [ Int 2; Int 2 ] ],
          [ 0; 1 ] ]
        |> List.iter (this.TestExpression ctx)

    [<TestMethod>]
    member this.TestMeasure() =
        let ctx =
            this.Context
            |> add_to_context
                "k"
                (Type.Ket [ Type.Int; Type.Bool ])
                (e.Ket(
                    e.Set
                        [ e.Tuple [ e.Int 0; e.Bool true ]
                          e.Tuple [ e.Int 0; e.Bool false ]
                          e.Tuple [ e.Int 1; e.Bool true ] ]
                ))

        [ e.Ket(e.Set []), []
          // Solve (k, k.0 == 2)
          e.Solve(e.Var "k", e.Equals(e.Project(e.Var "k", e.Int 0), e.Int 2)), []
          // k
          e.Var "k",
          [
            // Looks like because they are set, they are ordered differently from inputs:
            // this might be problematic for tests...
            Tuple [ Int 0; Bool false ]
            Tuple [ Int 0; Bool true ]
            Tuple [ Int 1; Bool true ] ]
          // k.1
          e.Project(e.Var "k", e.Int 1), [ Bool false; Bool true ]

          // (false, k.1)
          e.Join(e.Ket(e.Bool false), e.Project(e.Var "k", e.Int 1)),
          [ Tuple [ Bool false; Bool false ]; Tuple [ Bool false; Bool true ] ]

          // not (k.0 == 0 and k.1)
          e.Not(e.And(e.Equals(e.Project(e.Var "k", e.Int 0), e.Int 0), e.Project(e.Var "k", e.Int 1))),
          [ Bool false; Bool true ] ]
        |> List.iter (verify_expression ctx)

    [<TestMethod>]
    member this.TestRecursiveMethod() =
        let ctx = this.Context

        [
          // let sum (acc: Ket<Int>, set:Set<Int>) =
          //      if Count(set) == 0 then
          //          acc
          //      else
          //          let elem = Element(set)
          //          let rest = Remove(elem, set)
          //          sum(acc + elem, rest)
          // sum( |10, 20, 30>,  1 .. 4)
          e.Block(
              [ s.Let(
                    "sum",
                    e.Method(
                        arguments = [ ("acc", Type.Ket [ Type.Int ]); ("set", Type.Set Type.Int) ],
                        returns = Type.Ket [ Type.Int ],
                        body =
                            e.If(
                                (e.Equals(e.Count(e.Var "set"), e.Int 0),
                                 e.Var "acc",
                                 e.Block(
                                     [ s.Let("elem", e.Element(e.Var "set"))
                                       s.Let("rest", e.Remove(e.Var "elem", e.Var "set")) ],
                                     e.CallMethod(e.Var "sum", [ e.Add(e.Var "acc", e.Var "elem"); e.Var "rest" ])
                                 ))
                            )
                    )
                ) ],
              e.CallMethod(e.Var "sum", [ e.Ket(e.Set [ e.Int 10; e.Int 20; e.Int 30 ]); e.Range(e.Int 1, e.Int 4) ])
          ),
          [ Int 16; Int 26; Int 36 ] ]
        |> List.iter (verify_expression ctx)


    member this.TestExpression (ctx: EvalContext) (expr, state, columns) =
        let qpu = ctx.qpu
        // If it is not already a Prepare expression, wrap in Prepare...
        let e =
            match typecheck (expr, ctx.typeCtx) with
            | Ok (result, _) ->
                match result with
                | typed.E.Universe _ -> expr
                | _ -> e.Prepare expr
            | Error msg ->
                printfn "e: %A" expr
                Assert.AreEqual($"Expression failed type-checking.", $"Got Error msg: {msg}")
                expr

        match run (e, ctx) with
        | Ok (Universe universe, ctx) ->
            let universe = universe :?> aleph.runtime.qpu.classic.Universe
            let state' = universe.State
            let columns' = universe.Columns
            printfn "columns: %A\nmemory: %A\n" columns' state'
            Assert.AreEqual(state, state')
            Assert.AreEqual(columns, columns')
        | Ok (v, _) ->
            printfn "e: %A" e
            Assert.AreEqual($"Expecting Universe value.", $"Got {v}")
        | Error msg ->
            printfn "e: %A" e
            Assert.AreEqual($"Expecting valid expression.", $"Got Error msg: {msg}")


    member this.TestInvalidExpression ctx (e, error) =
        match run (e, ctx) with
        | Ok (v, _) -> Assert.AreEqual($"Expected error: {error}", $"Got Value: {v}")
        | Error msg -> Assert.AreEqual(error, msg)
