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
type TestQPUClassic () =
    member this.Context = { 
        ClassicValueContext.ctx with qpu = aleph.runtime.qpu.classic.Processor()
    }

    [<TestMethod>]
    member this.TestBasicExpressions () =
        let ctx = this.Context

        [
            // | false >
            u.Ket (u.Bool false), 
                [ [ Bool false ] ],
                [ 0 ]
            // | 0, 1, 2 >
            u.Ket (u.Set [u.Int 0; u.Int 1; u.Int 2]),
                [
                    [ Int 0 ]
                    [ Int 1 ]
                    [ Int 2 ]
                ],
                [ 0 ]
            // | (0,0), (0,1), (1,1) >
            u.Ket (u.Set [u.Tuple [u.Int 0; u.Int 0]; u.Tuple [u.Int 0; u.Int 1]; u.Tuple [u.Int 1; u.Int 1]]),
                [
                    [ Int 0; Int 0 ]
                    [ Int 0; Int 1 ]
                    [ Int 1; Int 1 ]
                ],
                [ 0; 1 ]
            // | (0,0,0), (0,1,1), (1,1,0), (1,1,2) >.2
            u.Project (u.Ket (u.Set [
                    u.Tuple [ u.Int 0; u.Int 0; u.Int 0 ]
                    u.Tuple [ u.Int 0; u.Int 1; u.Int 1 ]
                    u.Tuple [ u.Int 1; u.Int 1; u.Int 0 ]
                    u.Tuple [ u.Int 1; u.Int 1; u.Int 2 ]
                ]), u.Int 2),
                [
                    [ Int 0; Int 0; Int 0 ]
                    [ Int 0; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 0 ]
                    [ Int 1; Int 1; Int 2 ]
                ],
                [2]
            // ( | 0, 1 >, | 1, 2 > )
            u.Join (u.Ket (u.Set [u.Int 0; u.Int 1]), u.Ket (u.Set [u.Int 1; u.Int 2])),
                [
                    [ Int 0; Int 1 ]
                    [ Int 0; Int 2 ]
                    [ Int 1; Int 1 ]
                    [ Int 1; Int 2 ]
                ],
                [ 0; 1 ]
            u.KetAll (u.Int 3),
                [
                    [Int 0]
                    [Int 1]
                    [Int 2]
                    [Int 3]
                    [Int 4]
                    [Int 5]
                    [Int 6]
                    [Int 7]
                ],
                [ 0 ]
            u.Join( u.Ket (u.Set [ u.Tuple [u.Int 0; u.Int -6]]), u.KetAll (u.Int 2)),
            [
                    [Int 0; Int -6; Int 0]
                    [Int 0; Int -6; Int 1]
                    [Int 0; Int -6; Int 2]
                    [Int 0; Int -6; Int 3]
            ],
            [ 0; 1; 2]
        ]
        |> List.iter (this.TestExpression ctx)

        
    [<TestMethod>]
    member this.TestAddMultiply () =
        let ctx = 
            this.Context 
            |> add_to_context "k1" (AnyType.QType (QType.Ket [Type.Int; Type.Int])) (u.Ket (u.Set [
                u.Tuple [ u.Int 0; u.Int 0]
                u.Tuple [ u.Int 0; u.Int 1]
                u.Tuple [ u.Int 1; u.Int 1]
            ]))

        [
            // k1.0 + k1.1
            u.Add( u.Project (u.Var "k1", u.Int 0), u.Project (u.Var "k1", u.Int 1) ),
                [
                    [ Int 0; Int 0; Int 0 ]
                    [ Int 0; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 2 ]
                ],
                [ 2 ]
            // k1.0 + 1
            u.Add( u.Project (u.Var "k1", u.Int 0), u.Int 1),
                [
                    [ Int 0; Int 0; Int 1; Int 1 ]
                    [ Int 0; Int 1; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 1; Int 2 ]
                ],
                [ 3 ]
            // k1.0 * 5
            u.Multiply( u.Project (u.Var "k1", u.Int 0), u.Int 5),
                [
                    [ Int 0; Int 0; Int 5; Int 0 ]
                    [ Int 0; Int 1; Int 5; Int 0 ]
                    [ Int 1; Int 1; Int 5; Int 5 ]
                ],
                [ 3 ]
            // k1.0 + | 1, 2, 3 >
            u.Add(u.Project (u.Var "k1", u.Int 1), u.Ket (u.Set [u.Int 1; u.Int 2; u.Int 3])),
                [
                    [ Int 0; Int 0; Int 1; Int 1 ]
                    [ Int 0; Int 0; Int 2; Int 2 ]
                    [ Int 0; Int 0; Int 3; Int 3 ]
                    [ Int 0; Int 1; Int 1; Int 2 ]
                    [ Int 0; Int 1; Int 2; Int 3 ]
                    [ Int 0; Int 1; Int 3; Int 4 ]
                    [ Int 1; Int 1; Int 1; Int 2 ]
                    [ Int 1; Int 1; Int 2; Int 3 ]
                    [ Int 1; Int 1; Int 3; Int 4 ]
                ],
                [ 3 ]
            // Join (k1.0, k1.1 + | 1, 2, 3 >)
            u.Join (u.Project (u.Var "k1", u.Int 0), u.Add(u.Project (u.Var "k1", u.Int 1), u.Ket (u.Set [u.Int 1; u.Int 2; u.Int 3]))),
                [
                    [ Int 0; Int 0; Int 1; Int 1 ]
                    [ Int 0; Int 0; Int 2; Int 2 ]
                    [ Int 0; Int 0; Int 3; Int 3 ]
                    [ Int 0; Int 1; Int 1; Int 2 ]
                    [ Int 0; Int 1; Int 2; Int 3 ]
                    [ Int 0; Int 1; Int 3; Int 4 ]
                    [ Int 1; Int 1; Int 1; Int 2 ]
                    [ Int 1; Int 1; Int 2; Int 3 ]
                    [ Int 1; Int 1; Int 3; Int 4 ]
                ],
                [ 0; 3 ]
            // Join (k1.0, k1.1 * | 1, 2, 3 >)
            u.Join (u.Project (u.Var "k1", u.Int 0), u.Multiply(u.Project (u.Var "k1", u.Int 1), u.Ket (u.Set [u.Int 1; u.Int 2; u.Int 3]))),
                [
                    [ Int 0; Int 0; Int 1; Int 0 ]
                    [ Int 0; Int 0; Int 2; Int 0 ]
                    [ Int 0; Int 0; Int 3; Int 0 ]
                    [ Int 0; Int 1; Int 1; Int 1 ]
                    [ Int 0; Int 1; Int 2; Int 2 ]
                    [ Int 0; Int 1; Int 3; Int 3 ]
                    [ Int 1; Int 1; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 2; Int 2 ]
                    [ Int 1; Int 1; Int 3; Int 3 ]
                ],
                [ 0; 3 ]
        ]
        |> List.iter (this.TestExpression ctx)

    [<TestMethod>]
    member this.TestJoin () =
        let ctx = 
            this.Context
            |> add_to_context "k1" (AnyType.QType (QType.Ket [Type.Int; Type.Int])) (u.Ket (u.Set [
                u.Tuple [ u.Int 0; u.Int 0]
                u.Tuple [ u.Int 0; u.Int 1]
                u.Tuple [ u.Int 1; u.Int 1]
            ]))
            |> add_to_context "k2" (AnyType.QType (QType.Ket [Type.Int])) (u.Ket (u.Set [
                u.Tuple [ u.Int 1 ]
                u.Tuple [ u.Int 3 ]
            ]))

        [
            // Join (k1, k1)
            u.Join (u.Var "k1", u.Var "k1"),
                [
                    [ Int 0; Int 0]
                    [ Int 0; Int 1]
                    [ Int 1; Int 1]
                ],
                [ 0; 1; 0; 1 ]
            // Join (k1, k2)
            u.Join (u.Var "k1", u.Var "k2"),
                [
                    [ Int 0; Int 0; Int 1 ]
                    [ Int 0; Int 0; Int 3 ]
                    [ Int 0; Int 1; Int 1 ]
                    [ Int 0; Int 1; Int 3 ]
                    [ Int 1; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 3 ]
                ],
                [ 0; 1; 2 ]
            // (Join k1, |true, false>)
            u.Join (u.Var "k1", u.Ket (u.Set [u.Bool true; u.Bool false])),
                [
                    [ Int 0; Int 0; Bool false ]
                    [ Int 0; Int 0; Bool true ]
                    [ Int 0; Int 1; Bool false ]
                    [ Int 0; Int 1; Bool true ]
                    [ Int 1; Int 1; Bool false ]
                    [ Int 1; Int 1; Bool true ]
                ],
                [ 0; 1; 2 ]
            // let e1 = k1.1 + 10
            // let e2 = k2 + 10
            // (Join k1, e2 == e1 )
            u.Block (
                [
                    Let ("e1", u.Add (u.Project (u.Var "k1", u.Int 1), u.Int 10))
                    Let ("e2", u.Add (u.Var "k2", u.Int 10))
                ], u.Join (u.Var "k1", u.Equals(u.Var "e2", u.Var "e1"))),
                [
                    [ Int 0; Int 0; Int 1; Int 10; Int 11; Int 10; Int 10; Bool false ]
                    [ Int 0; Int 0; Int 3; Int 10; Int 13; Int 10; Int 10; Bool false ]
                    [ Int 0; Int 1; Int 1; Int 10; Int 11; Int 10; Int 11; Bool true ]
                    [ Int 0; Int 1; Int 3; Int 10; Int 13; Int 10; Int 11; Bool false ]
                    [ Int 1; Int 1; Int 1; Int 10; Int 11; Int 10; Int 11; Bool true ]
                    [ Int 1; Int 1; Int 3; Int 10; Int 13; Int 10; Int 11; Bool false ]
                ],
                [ 0; 1; 7 ]


            // let alpha = |3,5>
            // let x =
            //    let alpha = alpha * 10
            //    let x = 
            //      let alpha = alpha * 20
            //      alpha
            //    (alpha, x)
            // (alpha, x)
            u.Block ([
                s.Let ("alpha", u.Ket (u.Set [ u.Int 3; u.Int 5]))
                s.Let ("x",
                    u.Block ([
                        s.Let ("alpha", u.Multiply(u.Var "alpha", u.Int 10))
                        s.Let ("x",
                            u.Block ([
                                s.Let ("alpha", u.Multiply(u.Var "alpha", u.Int 20))
                            ], u.Var "alpha"))
                    ], u.Join (u.Var "alpha", u.Var "x")))
            ], u.Join (u.Var "alpha", u.Var "x")),
            [
                [Int 3; Int 10; Int 30; Int 20; Int 600]
                [Int 5; Int 10; Int 50; Int 20; Int 1000]
            ],
            [ 0; 2; 4]

        ]
        |> List.iter (this.TestExpression ctx)


    [<TestMethod>]
    member this.TestIndex () =
        let ctx = 
            this.Context 
            |> add_to_context "k1" (AnyType.QType (QType.Ket [Type.Int; Type.Int])) (u.Ket (u.Set [
                u.Tuple [ u.Int 0; u.Int 0]
                u.Tuple [ u.Int 0; u.Int 1]
                u.Tuple [ u.Int 1; u.Int 1]
            ]))

        [
            // k1.0 + k1.[0 + 1]
            u.Add( u.Project (u.Var "k1", u.Int 0), u.Project (u.Var "k1", u.Add (u.Int 0, u.Int 1))),
                [
                    [ Int 0; Int 0; Int 0 ]
                    [ Int 0; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 2 ]
                ],
                [ 2 ]
            // k1.0 + k1.[0 + 3]        // Index is modular, so 3 == 1
            u.Add( u.Project (u.Var "k1", u.Int 0), u.Project (u.Var "k1", u.Add (u.Int 0, u.Int 3))),
                [
                    [ Int 0; Int 0; Int 0 ]
                    [ Int 0; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 2 ]
                ],
                [ 2 ]
        ]
        |> List.iter (this.TestExpression ctx)

    [<TestMethod>]
    member this.TestIfQ () =
        let ctx = 
            this.Context
            |> add_to_context "k1" (AnyType.QType (QType.Ket [Type.Int; Type.Int])) (u.Ket (u.Set [
                u.Tuple [ u.Int 0; u.Int 0]
                u.Tuple [ u.Int 0; u.Int 1]
                u.Tuple [ u.Int 1; u.Int 1]
            ]))
            |> add_to_context "k2" (AnyType.QType (QType.Ket [Type.Int])) (u.Ket (u.Set [
                u.Tuple [ u.Int 2 ]
                u.Tuple [ u.Int 3 ]
            ]))

        [
            // if k1.1 == k1.0 then k1.1 else 42
            u.If (u.Equals ( u.Project (u.Var "k1", u.Int 1), u.Project (u.Var "k1", u.Int 0)),
                    u.Project (u.Var "k1", u.Int 1),
                    u.Int 42),
                [
                    [ Int 0; Int 0; Bool true; Int 42; Int 0 ]
                    [ Int 0; Int 1; Bool false; Int 42; Int 42 ]
                    [ Int 1; Int 1; Bool true; Int 42; Int 1 ]
                ],
                [ 4 ]
        ]
        |> List.iter (this.TestExpression ctx)

    [<TestMethod>]
    member this.TestIfClassic () =
        let ctx = 
            this.Context
            |> add_to_context "k1" (AnyType.QType (QType.Ket [Type.Int; Type.Int])) (u.Ket (u.Set [
                u.Tuple [ u.Int 0; u.Int 0]
                u.Tuple [ u.Int 0; u.Int 1]
                u.Tuple [ u.Int 1; u.Int 1]
            ]))
            |> add_to_context "k2" (AnyType.QType (QType.Ket [Type.Int])) (u.Ket (u.Set [
                u.Tuple [ u.Int 2 ]
                u.Tuple [ u.Int 3 ]
            ]))

        [
            // if true then k1.1 else 42
            u.If (u.Bool true,
                    u.Project (u.Var "k1", u.Int 1),
                    u.Int 42),
                [
                    [ Int 0; Int 0 ]
                    [ Int 0; Int 1 ]
                    [ Int 1; Int 1 ]
                ],
                [ 1 ]
            // if false then k1.1 else 42
            u.If (u.Bool false,
                    u.Project (u.Var "k1", u.Int 1),
                    u.Int 42),
                [
                    [ Int 42 ]
                ],
                [ 0 ]
        ]
        |> List.iter (this.TestExpression ctx)


    [<TestMethod>]
    member this.TestSolveEquals () =
        let ctx = 
            this.Context
            |> add_to_context "k1" (AnyType.QType (QType.Ket [Type.Int; Type.Int])) (u.Ket (u.Set [
                u.Tuple [ u.Int 0; u.Int 0]
                u.Tuple [ u.Int 0; u.Int 1]
                u.Tuple [ u.Int 1; u.Int 1]
            ]))
            |> add_to_context "k2" (AnyType.QType (QType.Ket [Type.Int])) (u.Ket (u.Set [
                u.Tuple [ u.Int 2 ]
                u.Tuple [ u.Int 3 ]
            ]))

        [
            // k1.1 == 1
            u.Equals ( u.Project (u.Var "k1", u.Int 1), u.Int 1),
                [
                    [ Int 0; Int 0; Int 1; Bool false ]
                    [ Int 0; Int 1; Int 1; Bool true ]
                    [ Int 1; Int 1; Int 1; Bool true ]
                ],
                [ 3 ]
            // (Solve k1, k1.1 == 1)
            u.Solve (u.Var "k1", u.Equals ( u.Project (u.Var "k1", u.Int 1), u.Int 1)),
                [
                    [ Int 0; Int 1; Int 1; Bool true ]
                    [ Int 1; Int 1; Int 1; Bool true ]
                ],
                [ 0; 1 ]
            // (Solve k1, k1.0 + k1.1 == 1)
            u.Solve (u.Var "k1", u.Equals ( u.Add( u.Project (u.Var "k1", u.Int 0), u.Project (u.Var "k1", u.Int 1) ), u.Int 1)),
                [
                    [ Int 0; Int 1; Int 1; Int 1; Bool true ]
                ],
                [ 0; 1 ]
            // (Solve k1.0, k1.1 + | 1, 2, 3 > == |2, 4> )
            u.Solve (u.Project (u.Var "k1", u.Int 1), u.Equals(u.Add(u.Project (u.Var "k1", u.Int 1), u.Ket (u.Set [u.Int 1; u.Int 2; u.Int 3])), u.Ket (u.Set [u.Int 2; u.Int 4]))),
                [
                    [ Int 0; Int 0; Int 2; Int 2; Int 2; Bool true ]
                    [ Int 0; Int 1; Int 1; Int 2; Int 2; Bool true ]
                    [ Int 0; Int 1; Int 3; Int 4; Int 4; Bool true ]
                    [ Int 1; Int 1; Int 1; Int 2; Int 2; Bool true ]
                    [ Int 1; Int 1; Int 3; Int 4; Int 4; Bool true ]
                ],
                [ 1 ]
        ]
        |> List.iter (this.TestExpression ctx)


    [<TestMethod>]
    member this.TestBoolOps () =
        let ctx = 
            this.Context 
            |> add_to_context "k" (AnyType.QType (QType.Ket [Type.Int; Type.Bool])) (u.Ket (u.Set [
                u.Tuple [ u.Int 0; u.Bool true]
                u.Tuple [ u.Int 0; u.Bool false]
                u.Tuple [ u.Int 1; u.Bool true]
            ]))

        [
            u.Var "k",
                [
                    // Looks like because they are set, they are ordered differently from inputs:
                    // this might be problematic for tests...
                    [ Int 0; Bool false; ]
                    [ Int 0; Bool true; ]
                    [ Int 1; Bool true; ]
                ],
                [ 0; 1 ]

            // not k.1
            u.Not ( u.Project (u.Var "k", u.Int 1)),
                [
                    [ Int 0; Bool false; Bool true ]
                    [ Int 0; Bool true; Bool false ]
                    [ Int 1; Bool true; Bool false ]
                ],
                [ 2 ]

            // (false or k.1)
                u.Or ( 
                    u.Bool false,
                    u.Project (u.Var "k", u.Int 1)),
                [
                    [ Bool false; Int 0; Bool false; Bool false ]
                    [ Bool false; Int 0; Bool true;  Bool true ]
                    [ Bool false; Int 1; Bool true;  Bool true ]
                ],
                [ 3 ]

            // not (k.0 == 0 and k.1)
            u.Not ( 
                u.And ( 
                    u.Equals ( 
                        u.Project (u.Var "k", u.Int 0), 
                        u.Int 0), 
                    u.Project (u.Var "k", u.Int 1)) ),
                [
                    [ Int 0; Bool false; Int 0; Bool true; Bool false; Bool true ]
                    [ Int 0; Bool true;  Int 0; Bool true; Bool true; Bool false ]
                    [ Int 1; Bool true;  Int 0; Bool false; Bool false; Bool true ]
                ],
                [ 5 ]
        ]
        |> List.iter (this.TestExpression ctx)


    [<TestMethod>]
    member this.TestCallMethod () =
        let ctx = this.Context

        [
            // // let colors() = |1,2,3>
            // // colors()
            u.Block (
                [
                    Let ("colors", u.Method([], u.Ket (u.Set [u.Int 1; u.Int 2; u.Int 3])))
                ],
                u.CallMethod (u.Var "colors", [])),
                [
                    [ Int 1 ]
                    [ Int 2 ]
                    [ Int 3 ]
                ],
                [ 0 ]
            // let colors() = |1,2,3>
            // ( colors(), colors() )
            u.Block (
                [
                    Let ("colors", u.Method([], u.Ket (u.Set [u.Int 1; u.Int 2; u.Int 3])))
                ],
                u.Join (u.CallMethod (u.Var "colors", []), u.CallMethod (u.Var "colors", []))),
                [
                    [ Int 1; Int 1 ]
                    [ Int 1; Int 2 ]
                    [ Int 1; Int 3 ]
                    [ Int 2; Int 1 ]
                    [ Int 2; Int 2 ]
                    [ Int 2; Int 3 ]
                    [ Int 3; Int 1 ]
                    [ Int 3; Int 2 ]
                    [ Int 3; Int 3 ]
                ],
                [ 0; 1 ]
            // let k1 = |0,1>
            // let add_one(k: Ket<Int>) = k + 1
            // add_one(k1)
            u.Block (
                [
                    Let ("k1", u.Ket (u.Set [u.Int 0; u.Int 1]))
                    Let ("add_one", u.Method(["k", (AnyType.QType (QType.Ket [Type.Int]))], u.Add(u.Var "k", u.Int 1)))
                ],
                u.CallMethod (u.Var "add_one", [u.Var "k1"])),
                [
                    [ Int 0; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 2 ]
                ],
                [ 2 ]
            // let k = |0,1>
            // let add_one(k: Ket<Int>) = k + 1
            // let k = add_one(k)
            // k
            u.Block (
                [
                    Let ("k", u.Ket (u.Set [u.Int 0; u.Int 1]))
                    Let ("add_one", u.Method(["k", (AnyType.QType (QType.Ket [Type.Int]))], u.Add(u.Var "k", u.Int 1)))
                    Let ("k", u.CallMethod (u.Var "add_one", [u.Var "k"]))
                ],
                u.Var "k"),
                [
                    [ Int 0; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 2 ]
                ],
                [ 2 ]
            // let k1 = |0,1>
            // let add_one(k: Ket<Int>) = k + 1
            // let k2 = add_one(k1)
            // let k1 = | 2,3 >
            // k2
            u.Block (
                [
                    Let ("k1", u.Ket (u.Set [u.Int 0; u.Int 1]))
                    Let ("add_one", u.Method(["k", (AnyType.QType (QType.Ket [Type.Int]))], u.Add(u.Var "k", u.Int 1)))
                    Let ("k2", u.CallMethod (u.Var "add_one", [u.Var "k1"]))
                    Let ("k1", u.Ket (u.Set [u.Int 2; u.Int 3]))
                ],
                u.Var "k2"),
                [
                    [ Int 0; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 2 ]
                ],
                [ 2 ]
            // let prepare_bell(a: int, b: int) = Prepare( |(a,a), (b,b)> )
            // prepare_bell(2, i1)
            u.Block (
                [
                    Let ("prepare_bell", u.Method(
                        ["a", AnyType.Type (Type.Int); "b", AnyType.Type (Type.Int)], 
                        u.Prepare (
                            u.Ket (u.Set [ u.Tuple [u.Var "a"; u.Var "a"]; u.Tuple [u.Var "b"; u.Var "b"] ]))))
                ],
                u.CallMethod (u.Var "prepare_bell", [u.Int 2; u.Var "i1"])),
                [
                    [ Int 1; Int 1 ]
                    [ Int 2; Int 2 ]
                ],
                [ 0; 1 ]
        ]
        |> List.iter (this.TestExpression ctx)

    [<TestMethod>]
    member this.TestMeasure () =
        let ctx = 
            this.Context 
            |> add_to_context "k" (AnyType.QType (QType.Ket [Type.Int; Type.Bool])) (u.Ket (u.Set [
                u.Tuple [ u.Int 0; u.Bool true]
                u.Tuple [ u.Int 0; u.Bool false]
                u.Tuple [ u.Int 1; u.Bool true]
            ]))

        [
            u.Ket (u.Set []),
                []
            // Solve (k, k.0 == 2)
            u.Solve (u.Var "k", u.Equals (u.Project (u.Var "k", u.Int 0), u.Int 2)),
                []
            // k
            u.Var "k",
                [
                    // Looks like because they are set, they are ordered differently from inputs:
                    // this might be problematic for tests...
                    Tuple [ Int 0; Bool false; ]
                    Tuple [ Int 0; Bool true; ]
                    Tuple [ Int 1; Bool true; ]
                ]
            // k.1
            u.Project (u.Var "k", u.Int 1),
                [
                    Bool false
                    Bool true
                ]

            // (false, k.1)
                u.Join ( 
                    u.Ket (u.Bool false),
                    u.Project (u.Var "k", u.Int 1)),
                [
                    Tuple [ Bool false; Bool false ]
                    Tuple [ Bool false; Bool true ]
                ]

            // not (k.0 == 0 and k.1)
            u.Not ( 
                u.And ( 
                    u.Equals ( 
                        u.Project (u.Var "k", u.Int 0), 
                        u.Int 0), 
                    u.Project (u.Var "k", u.Int 1)) ),
                [
                    Bool false;
                    Bool true;
                ]
        ]
        |> List.iter (verify_expression ctx)


    member this.TestExpression (ctx: EvalContext) (e, state, columns)=
        let qpu = ctx.qpu
        // If it is not already a Prepare expression, wrap in Prepare...
        let e = 
            match typecheck(e, ctx.typeCtx) with
            | Ok (result, _) ->
                match result with 
                | typed.E.Universe (_, UType.Universe _) -> e
                | _ -> u.Prepare e
            | Error msg ->
                printfn "e: %A" e
                Assert.AreEqual($"Expression failed type-checking.", $"Got Error msg: {msg}")
                e
        match run(e, ctx) with
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
        | Ok (v, _) ->
            Assert.AreEqual($"Expected error: {error}", $"Got Value: {v}")
        | Error msg -> 
            Assert.AreEqual(error, msg)
