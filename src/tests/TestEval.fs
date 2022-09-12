namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.parser.ast
open aleph.parser.ast.typed
open aleph.runtime.Eval


module ClassicValueContext =
    let ctx =
        {
            qpu = { new QPU with
                member this.Measure(arg1: IUniverse): Result<Value,string> = failwith "Not Implemented"
                member this.Prepare(arg1: U, arg2: EvalContext): Result<(Value * EvalContext),string> =  failwith "Not Implemented"
            }
            heap =  Map [
                "i1", Int 1
                "b1", Bool true
                "t1", Tuple [Bool false; Int 1]
                "t2", Tuple [Bool true; Int 2]
                "t3", Tuple [Int 0; Int 1; Int 2]
                "s1", Set (Set.ofList  [
                    Tuple [Bool false; Int 0]
                    Tuple [Bool false; Int 1]
                    Tuple [Bool false; Int 2]
                ])
                // () -> (1, 2)
                "m0", Method ([], (E.Classic (C.Tuple [C.IntLiteral 1; C.IntLiteral 2], Type.Tuple [Type.Int; Type.Int])))
                // (a, b) -> a + b
                "m1", Method (["a"; "b"], (E.Classic (C.Add ((C.Var "a", C.Var "b")), Type.Int)))
                // (x, y, z) -> y || x[z]
                "m2", Method (["x"; "y"; "z"], (E.Classic (C.Or ((C.Var "y", C.Index (C.Var ("x"), C.Var "z"))), Type.Bool)))
            ]
            typeCtx = {
                heap = Map [ 
                    "i1", AnyType.Type Type.Int
                    "b1", AnyType.Type Type.Bool
                    "t1", AnyType.Type (Type.Tuple [Type.Bool; Type.Int])
                    "t2", AnyType.Type (Type.Tuple [Type.Bool; Type.Int])
                    "t3", AnyType.Type (Type.Tuple [Type.Int; Type.Int; Type.Int])
                    "s1", AnyType.Type (Type.Set (Type.Tuple [Type.Bool; Type.Int]))
                    "m0", AnyType.Type (Type.Method ([], (AnyType.Type (Type.Tuple [Type.Int; Type.Int]))))
                    "m1", AnyType.Type (Type.Method ([AnyType.Type Type.Int; AnyType.Type Type.Int], AnyType.Type Type.Int))
                    "m2", AnyType.Type (Type.Method ([AnyType.Type Type.Tuple[Type.Int; Type.Bool; Type.Bool]; AnyType.Type Type.Bool; AnyType.Type Type.Int], AnyType.Type Type.Bool))
                ]
                previousCtx = None
            }
            callerCtx = None
        }

[<TestClass>]
type TestEval () =

    member this.TestExpression ctx (e, v)=
        printfn "e: %A" e
        match run (e, ctx) with
        | Ok (v', _) -> 
            Assert.AreEqual(v, v')
        | Error msg -> 
            Assert.AreEqual($"Expecting Value {v}", $"Got Error msg: {msg}")

    member this.TestInvalidExpression ctx (e, error) =
        match run (e, ctx) with
        | Ok (v, _) ->
            Assert.AreEqual($"Expected error: {error}", $"Got Value: {v}")
        | Error msg -> 
            Assert.AreEqual(error, msg)

    [<TestMethod>]
    member this.TestClassicLiterals () =
        let ctx = ClassicValueContext.ctx

        [
            // false
            e.Bool false, 
                Value.Bool false
            // 5
            e.Int 5, 
                Value.Int 5
            // (false, 0, 1)
            e.Tuple [e.Bool false; e.Int 0; e.Int 1],
                Value.Tuple [Bool false; Int 0; Int 1]

            // {}
            e.Set [],
                Value.Set (Set.empty)

            // {false}
            e.Set [e.Bool false],
                Value.Set (Set.ofList [Bool false])

            // {0, 1, 2}
            e.Set [e.Int 0; e.Int 1; e.Int 2],
                Value.Set (Set.ofList [Int 0; Int 1; Int 2])

            // {(false, 0, 0), (true, 0, 1), (true, 1, 1)}
            e.Set [
                e.Tuple [e.Bool false; e.Int 0; e.Int 0]
                e.Tuple [e.Bool true; e.Int 0; e.Int 1]
                e.Tuple [e.Bool true; e.Int 1; e.Int 1]
            ],
                Value.Set (Set.ofList [
                    Tuple [Bool false; Int 0; Int 0]
                    Tuple [Bool true; Int 0; Int 1]
                    Tuple [Bool true; Int 1; Int 1]])
            // { 1..5 }
            e.Range (e.Int 1, e.Int 5),
                Value.Set (Set.ofList [Int 1; Int 2; Int 3; Int 4])
            // (a: Int) -> a + 1
            e.Method (["a", AnyType.Type Type.Int], (e.Add (e.Var "a", e.Int 1))),
                Value.Method (["a"], (E.Classic (C.Add ((C.Var "a", C.IntLiteral 1)), Type.Int)))
            // (k1: Ket<Int>, k2: Ket<Int, Bool>) -> (k1, k2)
            e.Method (["k1", AnyType.QType (QType.Ket [Type.Int]); "k2", AnyType.QType (QType.Ket [Type.Int; Type.Bool])], 
                (e.Join (e.Var "k1", e.Var "k2"))),
                Value.Method (["k1"; "k2"], (E.Quantum (Q.Join (Q.Var "k1", Q.Var "k2"), (QType.Ket [Type.Int; Type.Int; Type.Bool]))))
            // (k1: Ket<Int>, k2: Ket<Int, Bool>) -> Prepare(k2)
            e.Method (["k1", AnyType.QType (QType.Ket [Type.Int]); "k2", AnyType.QType (QType.Ket [Type.Int; Type.Bool])], 
                (e.Prepare (e.Var "k2"))),
                Value.Method (["k1"; "k2"], (E.Universe (U.Prepare (Q.Var "k2"), (UType.Universe [Type.Int; Type.Bool]))))
        ]
        |> List.iter (this.TestExpression ctx)

        [
            // Type check:
            e.Set [ e.Tuple [ e.Int 0; e.Int 0]; e.Int 1], "All elements in a set must be of the same type."
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestBinaryExpressions () =
        let ctx = ClassicValueContext.ctx

        [
            // true || true
            e.Or (e.Bool true, e.Bool true),
                Value.Bool true
            // false || true
            e.Or (e.Bool false, e.Bool true),
                Value.Bool true
            // false && true
            e.And (e.Bool false, e.Bool true),
                Value.Bool false
            // false && false
            e.And (e.Bool false, e.Bool false),
                Value.Bool false
            // (5 + 10) * 4
            e.Multiply (e.Add(e.Int 5, e.Int 10), e.Int 4),
                Value.Int 60
            // (5 + 10) * 4 == 100
            e.Equals(e.Multiply (e.Add(e.Int 5, e.Int 10), e.Int 4), e.Int 100),
                Value.Bool false
            // (5 + 10) * 4 == 60
            e.Equals(e.Multiply (e.Add(e.Int 5, e.Int 10), e.Int 4), e.Int 60),
                Value.Bool true
            // (5 + 10) * 4 == 100 || true
            e.Or (e.Equals(e.Multiply (e.Add(e.Int 5, e.Int 10), e.Int 4), e.Int 100), e.Bool true),
                Value.Bool true
            // (5 + 10) * 4 < 50
            e.LessThan (e.Multiply (e.Add(e.Int 5, e.Int 10), e.Int 4), e.Int 50),
                Value.Bool false
            // (5 + 10) * 4 < 100
            e.LessThan (e.Multiply (e.Add(e.Int 5, e.Int 10), e.Int 4), e.Int 100),
                Value.Bool true
            // not ((5 + 10) * 4 < 50)
            e.Not (e.LessThan (e.Multiply (e.Add(e.Int 5, e.Int 10), e.Int 4), e.Int 50)),
                Value.Bool true
        ]
        |> List.iter (this.TestExpression ctx)

    [<TestMethod>]
    member this.TestIfExpressions () =
        let ctx = ClassicValueContext.ctx

        [
            // if true then 10 else 20
            e.If (e.Bool true, e.Int 10, e.Int 20),
                Value.Int 10
            // if false then 10 else 20 + 20
            e.If (e.Bool false, e.Int 10, e.Add (e.Int 20, e.Int 20)),
                Value.Int 40
        ]
        |> List.iter (this.TestExpression ctx)

        [
            // if 1 then 10 else 20
            e.If (e.Int 1, e.Int 10, e.Int 20),  "If condition must be a boolean, got Int"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestProjectExpressions () =
        let ctx = ClassicValueContext.ctx

        [
            // t1[0]
            e.Project (e.Var "t1", e.Int 0),
                Value.Bool false
            // t2[1]
            e.Project (e.Var "t2", e.Int 1),
                Value.Int 2
            // t3[0 + t2[1]]
            e.Project (e.Var "t3", e.Add(e.Int 0, e.Project (e.Var "t2", e.Int 1))),
                Value.Int 2
            // (t3, t2)[2]
            e.Project(e.Join (e.Var "t3", e.Var "t2"), e.Int 1),
                Value.Int 1
            // (t3, t2)[3]
            e.Project(e.Join (e.Var "t3", e.Var "t2"), e.Int 3),
                Value.Bool true
            // (t3, t3)[2 + 2]
            e.Project(e.Join (e.Var "t3", e.Var "t3"), e.Add(e.Int 2, e.Int 2)),
                Value.Int 1
        ]
        |> List.iter (this.TestExpression ctx)

        [
            // t1[t2[0]]
            e.Project (e.Var "t2", e.Project (e.Var "t2", e.Int 0)),  "Invalid projection index. Expected int expression, got: Classic (Project (Var \"t2\", 0), Bool)"
        ]
        |> List.iter (this.TestInvalidExpression ctx)

    [<TestMethod>]
    member this.TestCallMethodExpressions () =
        let ctx = ClassicValueContext.ctx

        [
            // m0 ()
            e.CallMethod (e.Var "m0", []),
                Value.Tuple [Value.Int 1; Value.Int 2]
            // m1 (10, 20)
            e.CallMethod (e.Var "m1", [e.Int 10; e.Int 20]),
                Value.Int 30
            // m2 (10, true, 0)
            e.CallMethod (e.Var "m2", [e.Tuple [e.Int 10; e.Bool false; e.Bool true]; e.Bool false; e.Int 2]),
                Value.Bool true
            // m2 (10, false, 0)
            e.CallMethod (e.Var "m2", [e.Tuple [e.Int 10; e.Bool false; e.Bool true]; e.Bool false; e.Int 1]),
                Value.Bool false
        ]
        |> List.iter (this.TestExpression ctx)


    [<TestMethod>]
    member this.TestSetExpressions () =
        let ctx = ClassicValueContext.ctx

        [
            // (Append 3, {})
            e.Append (e.Int 3, e.Set []),
                Value.Set (Set.ofList [Int 3])
            // (Append false, {false})
            e.Append (e.Bool false, e.Set [e.Bool false]),
                Value.Set (Set.ofList [Bool false])
            // (Append 1, {0, 4, 2})
            e.Append( e.Int 1, e.Set [e.Int 0; e.Int 4; e.Int 2]),
                Value.Set (Set.ofList [Int 0; Int 1; Int 2; Int 4])

            // (Remove false, {false})
            e.Remove (e.Bool false, e.Set [e.Bool false]),
                Value.Set (Set.ofList [])
            // (Remove 4, {0, 4, 2})
            e.Remove (e.Int 4, e.Set [e.Int 0; e.Int 4; e.Int 2]),
                Value.Set (Set.ofList [Int 0; Int 2])
            // (Remove (1,2), {(0,0), (1,1), (1,2)})
            e.Remove (e.Tuple [e.Int 1; e.Int 2], e.Set [
                e.Tuple [e.Int 0; e.Int 0]
                e.Tuple [e.Int 1; e.Int 1]
                e.Tuple [e.Int 1; e.Int 2]
            ]),
                Value.Set (Set.ofList [
                    Tuple [Int 0; Int 0]
                    Tuple [Int 1; Int 1]
                ])
            
            // (Count {})
            e.Count (e.Set []),
                Int 0
            // (Count {false})
            e.Count (e.Set [e.Bool false]),
                Int 1
            // ({false, false, true})
            e.Count (e.Set [e.Bool false; e.Bool false; e.Bool true]),
                Int 2
            // (Count {(0,0), (1,1), (1,2)})
            e.Count (e.Set [
                e.Tuple [e.Int 0; e.Int 0]
                e.Tuple [e.Int 1; e.Int 1]
                e.Tuple [e.Int 1; e.Int 2]
            ]),
                Int 3

            // (Remove(Element s1), s1), Count(s1)) 
            e.Tuple [e.Count (e.Remove (e.Element (e.Var "s1"), e.Var "s1")); e.Count(e.Var "s1")],
                Tuple [Int 2; Int 3]
            // (Count {(0,0), (1,1), (1,2)})
            e.Element (
                e.Remove (e.Tuple [e.Int 1; e.Int 1],
                    e.Remove (e.Tuple [e.Int 0; e.Int 0], 
                        e.Set [
                            e.Tuple [e.Int 0; e.Int 0]
                            e.Tuple [e.Int 1; e.Int 1]
                            e.Tuple [e.Int 1; e.Int 2]
                        ]))),
                Tuple [Int 1; Int 2]

        ]
        |> List.iter (this.TestExpression ctx)


    [<TestMethod>]
    member this.TestVariableScope () =
        let ctx = ClassicValueContext.ctx

        [
            // let x = (1, 2)
            // x
            e.Block ([
                s.Let ("x", e.Tuple [e.Int 1; e.Int 2])
            ], (e.Var "x")),
                Value.Tuple [Value.Int 1; Value.Int 2]

            // let x = 10
            // let y = x
            // let x = false
            // (x, y)
            e.Block ([
                s.Let ("x", e.Int 10)
                s.Let ("y", e.Var "x")
                s.Let ("x", e.Bool false)
            ], e.Tuple [e.Var "x"; e.Var "y"]),
                Value.Tuple [Value.Bool false; Value.Int 10]
            
            // let x =
            //    if true then 
            //      let y = 10
            //      y
            //    else
            //      let z = 20
            //      z
            // x
            e.Block ([
                s.Let ("x",
                    e.If (e.Bool true, 
                        e.Block ([
                            s.Let ("y", e.Int 10)
                        ], e.Var "y"),
                        e.Block ([
                            s.Let ("z", e.Int 20)
                        ], e.Var "z")))
            ], e.Var "x"),
                Value.Int 10

            // let foo = 100
            // let x =
            //    if false then 
            //      let y = 10
            //      y
            //    else
            //      foo
            // x
            e.Block ([
                s.Let ("foo", e.Int 100)
                s.Let ("x",
                    e.If (e.Bool false, 
                        e.Block ([
                            s.Let ("y", e.Int 10)
                        ], e.Var "y"),
                        e.Var "foo"))
            ], e.Var "x"),
                Value.Int 100

            // let alpha = 1
            // let x =
            //    if true then 
            //      let alpha = 10
            //      alpha
            //    else
            //      let alpha = 20
            //      alpha
            // (alpha, x)
            e.Block ([
                s.Let ("alpha", e.Int 1)
                s.Let ("x",
                    e.If (e.Bool true, 
                        e.Block ([
                            s.Let ("alpha", e.Int 10)
                        ], e.Var "alpha"),
                        e.Block ([
                            s.Let ("alpha", e.Int 20)
                        ], e.Var "alpha")))
            ], e.Tuple [e.Var "alpha"; e.Var "x"]),
                Value.Tuple [Value.Int 1; Value.Int 10]

            // let alpha = 1
            // let x =
            //    if false then 
            //      let alpha = 10
            //      alpha
            //    else
            //      let alpha = 20
            //      alpha
            // (alpha, x)
            e.Block ([
                s.Let ("alpha", e.Int 1)
                s.Let ("x",
                    e.If (e.Bool false, 
                        e.Block ([
                            s.Let ("alpha", e.Int 10)
                        ], e.Var "alpha"),
                        e.Block ([
                            s.Let ("alpha", e.Int 20)
                        ], e.Var "alpha")))
            ], e.Tuple [e.Var "alpha"; e.Var "x"]),
                Value.Tuple [Value.Int 1; Value.Int 20]


            // let alpha = 1
            // let beta = 10
            // let x =
            //    let alpha = alpha * beta
            //    let beta = 20
            //    let x = 
            //      let alpha = alpha * beta
            //      alpha
            //    (alpha, beta, x)
            // (Join (alpha, beta), x)
            e.Block ([
                s.Let ("alpha", e.Int 1)
                s.Let ("beta", e.Int 10)
                s.Let ("x", e.Block ([
                    s.Let ("alpha", e.Multiply(e.Var "alpha", e.Var "beta"))
                    s.Let ("beta", e.Int 20)
                    s.Let ("x", e.Block ([
                        s.Let ("alpha", e.Multiply(e.Var "alpha", e.Var "beta"))
                    ], e.Var "alpha"))
                ], e.Tuple [e.Var "alpha"; e.Var "beta"; e.Var "x"]))
            ], e.Join (e.Tuple [e.Var "alpha"; e.Var "beta"], e.Var "x")),
                Value.Tuple [Value.Int 1; Value.Int 10; Value.Int 10; Value.Int 20; Value.Int 200]

        ]
        |> List.iter (this.TestExpression ctx)


        [
            // let x =
            //    if true then 
            //      let y = 10
            //      y
            //    else
            //      let z = 20
            //      z
            // (x, y)
            e.Block ([
                s.Let ("x",
                    e.If (e.Bool true, 
                        e.Block ([
                            s.Let ("y", e.Int 10)
                        ], e.Var "y"),
                        e.Block ([
                            s.Let ("z", e.Int 20)
                        ], e.Var "z")))
            ], e.Tuple [e.Var "x"; e.Var "y"]), "Variable not found: y"

            // let alpha = 1
            // let x() = alpha
            // x()
            e.Block ([
                s.Let ("alpha", e.Int 1)
                s.Let ("x", e.Method([], e.Var "alpha"))
            ], e.CallMethod (e.Var "x", [])), "Variable not found: alpha"
        ]
        |> List.iter (this.TestInvalidExpression ctx)