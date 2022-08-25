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
                member this.Prepare(arg1: U, arg2: ValueContext): Result<(Value * ValueContext),string> =  failwith "Not Implemented"
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
                // (x, y, z) -> y || t1[z]
                "m2", Method (["x"; "y"; "z"], (E.Classic (C.Or ((C.Var "y", C.Index (C.Var ("t1"), C.Var "z"))), Type.Bool)))
            ]
            types = aleph.parser.TypeChecker.TypeContext [ 
                "i1", AnyType.Type Type.Int
                "b1", AnyType.Type Type.Bool
                "t1", AnyType.Type (Type.Tuple [Type.Bool; Type.Int])
                "t2", AnyType.Type (Type.Tuple [Type.Bool; Type.Int])
                "t3", AnyType.Type (Type.Tuple [Type.Int; Type.Int; Type.Int])
                "s1", AnyType.Type (Type.Set (Type.Tuple [Type.Bool; Type.Int]))
                "m0", AnyType.Type (Type.Method ([], (AnyType.Type (Type.Tuple [Type.Int; Type.Int]))))
                "m1", AnyType.Type (Type.Method ([AnyType.Type Type.Int; AnyType.Type Type.Int], AnyType.Type Type.Int))
                "m2", AnyType.Type (Type.Method ([AnyType.Type Type.Int; AnyType.Type Type.Bool; AnyType.Type Type.Int], AnyType.Type Type.Bool))
            ]
        }

[<TestClass>]
type TestEvalClassic () =

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
            u.Bool false, 
                Value.Bool false
            // 5
            u.Int 5, 
                Value.Int 5
            // (false, 0, 1)
            u.Tuple [u.Bool false; u.Int 0; u.Int 1],
                Value.Tuple [Bool false; Int 0; Int 1]

            // {}
            u.Set [],
                Value.Set (Set.ofList [])

            // {false}
            u.Set [u.Bool false],
                Value.Set (Set.ofList [Bool false])

            // {0, 1, 2}
            u.Set [u.Int 0; u.Int 1; u.Int 2],
                Value.Set (Set.ofList [Int 0; Int 1; Int 2])

            // {(false, 0, 0), (true, 0, 1), (true, 1, 1)}
            u.Set [
                u.Tuple [u.Bool false; u.Int 0; u.Int 0]
                u.Tuple [u.Bool true; u.Int 0; u.Int 1]
                u.Tuple [u.Bool true; u.Int 1; u.Int 1]
            ],
                Value.Set (Set.ofList [
                    Tuple [Bool false; Int 0; Int 0]
                    Tuple [Bool true; Int 0; Int 1]
                    Tuple [Bool true; Int 1; Int 1]])
            // { 1..5 }
            u.Range (u.Int 1, u.Int 5),
                Value.Set (Set.ofList [Int 1; Int 2; Int 3; Int 4])
            // (a: Int) -> a + 1
            u.Method (["a", AnyType.Type Type.Int], (u.Add (u.Var "a", u.Int 1))),
                Value.Method (["a"], (E.Classic (C.Add ((C.Var "a", C.IntLiteral 1)), Type.Int)))
            // (k1: Ket<Int>, k2: Ket<Int, Bool>) -> (k1, k2)
            u.Method (["k1", AnyType.QType (QType.Ket [Type.Int]); "k2", AnyType.QType (QType.Ket [Type.Int; Type.Bool])], 
                (u.Join (u.Var "k1", u.Var "k2"))),
                Value.Method (["k1"; "k2"], (E.Quantum (Q.Join (Q.Var "k1", Q.Var "k2"), (QType.Ket [Type.Int; Type.Int; Type.Bool]))))
            // (k1: Ket<Int>, k2: Ket<Int, Bool>) -> Prepare(k2)
            u.Method (["k1", AnyType.QType (QType.Ket [Type.Int]); "k2", AnyType.QType (QType.Ket [Type.Int; Type.Bool])], 
                (u.Prepare (u.Var "k2"))),
                Value.Method (["k1"; "k2"], (E.Universe (U.Prepare (Q.Var "k2"), (UType.Universe [Type.Int; Type.Bool]))))
        ]
        |> List.iter (this.TestExpression ctx)

        [
            // Type check:
            u.Set [ u.Tuple [ u.Int 0; u.Int 0]; u.Int 1], "All elements in a set must be of the same type."
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestBinaryExpressions () =
        let ctx = ClassicValueContext.ctx

        [
            // true || true
            u.Or (u.Bool true, u.Bool true),
                Value.Bool true
            // false || true
            u.Or (u.Bool false, u.Bool true),
                Value.Bool true
            // false && true
            u.And (u.Bool false, u.Bool true),
                Value.Bool false
            // false && false
            u.And (u.Bool false, u.Bool false),
                Value.Bool false
            // (5 + 10) * 4
            u.Multiply (u.Add(u.Int 5, u.Int 10), u.Int 4),
                Value.Int 60
            // (5 + 10) * 4 == 100
            u.Equals(u.Multiply (u.Add(u.Int 5, u.Int 10), u.Int 4), u.Int 100),
                Value.Bool false
            // (5 + 10) * 4 == 60
            u.Equals(u.Multiply (u.Add(u.Int 5, u.Int 10), u.Int 4), u.Int 60),
                Value.Bool true
            // (5 + 10) * 4 == 100 || true
            u.Or (u.Equals(u.Multiply (u.Add(u.Int 5, u.Int 10), u.Int 4), u.Int 100), u.Bool true),
                Value.Bool true
            // (5 + 10) * 4 < 50
            u.LessThan (u.Multiply (u.Add(u.Int 5, u.Int 10), u.Int 4), u.Int 50),
                Value.Bool false
            // (5 + 10) * 4 < 100
            u.LessThan (u.Multiply (u.Add(u.Int 5, u.Int 10), u.Int 4), u.Int 100),
                Value.Bool true
            // not ((5 + 10) * 4 < 50)
            u.Not (u.LessThan (u.Multiply (u.Add(u.Int 5, u.Int 10), u.Int 4), u.Int 50)),
                Value.Bool true
        ]
        |> List.iter (this.TestExpression ctx)

    [<TestMethod>]
    member this.TestIfExpressions () =
        let ctx = ClassicValueContext.ctx

        [
            // if true then 10 else 20
            u.If (u.Bool true, u.Int 10, u.Int 20),
                Value.Int 10
            // if false then 10 else 20 + 20
            u.If (u.Bool false, u.Int 10, u.Add (u.Int 20, u.Int 20)),
                Value.Int 40
        ]
        |> List.iter (this.TestExpression ctx)

        [
            // if 1 then 10 else 20
            u.If (u.Int 1, u.Int 10, u.Int 20),  "If condition must be a boolean, got Int"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestProjectExpressions () =
        let ctx = ClassicValueContext.ctx

        [
            // t1[0]
            u.Project (u.Var "t1", u.Int 0),
                Value.Bool false
            // t2[1]
            u.Project (u.Var "t2", u.Int 1),
                Value.Int 2
            // t3[0 + t2[1]]
            u.Project (u.Var "t3", u.Add(u.Int 0, u.Project (u.Var "t2", u.Int 1))),
                Value.Int 2
            // (t3, t2)[2]
            u.Project(u.Join (u.Var "t3", u.Var "t2"), u.Int 1),
                Value.Int 1
            // (t3, t2)[3]
            u.Project(u.Join (u.Var "t3", u.Var "t2"), u.Int 3),
                Value.Bool true
            // (t3, t3)[2 + 2]
            u.Project(u.Join (u.Var "t3", u.Var "t3"), u.Add(u.Int 2, u.Int 2)),
                Value.Int 1
        ]
        |> List.iter (this.TestExpression ctx)

        [
            // t1[t2[0]]
            u.Project (u.Var "t2", u.Project (u.Var "t2", u.Int 0)),  "Invalid projection index. Expected int expression, got: Classic (Project (Var \"t2\", 0), Bool)"
        ]
        |> List.iter (this.TestInvalidExpression ctx)

    [<TestMethod>]
    member this.TestCallMethodExpressions () =
        let ctx = ClassicValueContext.ctx

        [
            // m0 ()
            u.CallMethod (u.Var "m0", []),
                Value.Tuple [Value.Int 1; Value.Int 2]
            // m1 (10, 20)
            u.CallMethod (u.Var "m1", [u.Int 10; u.Int 20]),
                Value.Int 30
            // m2 (10, true, 0)
            u.CallMethod (u.Var "m2", [u.Int 10; u.Bool true; u.Int 0]),
                Value.Bool true
            // m2 (10, false, 0)
            u.CallMethod (u.Var "m2", [u.Int 10; u.Bool false; u.Int 0]),
                Value.Bool false
        ]
        |> List.iter (this.TestExpression ctx)


