namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.parser.ast
open aleph.parser.ast.typed
open aleph.parser.TypeChecker

// alias for untyped expressions
type u = Expression

[<TestClass>]
type TestCore () =

    member this.TestClassicExpression ctx (e, t, r)=
        match typecheck (e, ctx) with
        | Ok (Classic (r', t'), _) -> 
            Assert.AreEqual(r, r')
            Assert.AreEqual(t, t')
        | Ok (Quantum (r', t'), _) -> 
            Assert.AreEqual($"Classic {r}:{t}", $"Quantum {r'}: {t'}")
        | Error msg -> 
            Assert.AreEqual($"Classic {r}:{t}", $"Error msg: {msg}")

    member this.TestQuantumExpression ctx (e, t, r)=
        match typecheck (e, ctx) with
        | Ok (Classic (r', t'), _) -> 
            Assert.AreEqual($"Quantum {r}:{t}", $"Classic {r'}: {t'}")
        | Ok (Quantum (r', t'), _) -> 
            Assert.AreEqual(r, r')
            Assert.AreEqual(t, t')
        | Error msg -> 
            Assert.AreEqual($"Quantum {r}:{t}", $"Error msg: {msg}")

    member this.TestInvalidExpression ctx (e, error) =
        match typecheck (e, ctx) with
        | Ok (v, _) ->
            Assert.AreEqual($"Expected error: {error}", $"got {v}")
        | Error msg -> 
            Assert.AreEqual(error, msg)

    member this.TypeContext =

        TypeContext [ 
            "i1", AnyType.Type Type.Int
            "b1", AnyType.Type Type.Bool
            "t1", AnyType.Type (Type.Tuple [Type.Bool; Type.Int])
            "t2", AnyType.Type (Type.Tuple [Type.Bool; Type.Int])
            "s1", AnyType.Type (Type.Set (Type.Tuple [Type.Bool; Type.Int]))
            "qb1", AnyType.QType QBool
            "k1", AnyType.QType QInt
            "k2", AnyType.QType (QType.Ket [Type.Int; Type.Bool])
            "m1", AnyType.Type (Type.Method ([], AnyType.Type Type.Int))
            "q1", AnyType.Type (Type.Method ([], AnyType.QType QType.Ket[Type.Int]))
        ]


    [<TestMethod>]
    member this.TestBoolInt () =
        let ctx = Map.empty

        [
            u.Bool false, Type.Bool, C.BoolLiteral false
            u.Int 5, Type.Int, C.IntLiteral 5
        ]
        |> List.iter (this.TestClassicExpression ctx)


    [<TestMethod>]
    member this.TestVar () =
        let ctx = this.TypeContext

        [
            u.Var "i1", Type.Int, C.Var "i1"
            u.Var "m1", Type.Method ([], Type Type.Int), C.Var "m1"
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            u.Var "qb1", QBool, Q.Var "qb1"
            u.Var "k1", QInt, Q.Var "k1"
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            u.Var "foo", "Unknown variable: foo"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestTuple () =
        let ctx = this.TypeContext

        [
            // ()
            u.Tuple [], Type.Tuple [], C.Tuple []
            // (3)
            u.Tuple [u.Int 3], Type.Tuple [Type.Int], C.Tuple [C.IntLiteral 3]
            // (3,5)
            u.Tuple [u.Int 3; u.Int 5], 
                Type.Tuple [Type.Int; Type.Int],
                C.Tuple [C.IntLiteral 3; C.IntLiteral 5]
            // (f, b1, t)
            u.Tuple [u.Bool false; u.Var "b1"; u.Bool true], 
                Type.Tuple [Type.Bool; Type.Bool; Type.Bool], 
                C.Tuple [C.BoolLiteral false; C.Var "b1"; C.BoolLiteral true]
            // (i1, i1)
            u.Tuple [u.Var "i1"; u.Var "i1"], 
                Type.Tuple [Type.Int; Type.Int], 
                C.Tuple [C.Var "i1"; C.Var "i1"]
            // (i1, b1, 42)
            u.Tuple [u.Var "i1"; u.Var "b1"; u.Int 42], 
                Type.Tuple [Type.Int; Type.Bool; Type.Int], 
                C.Tuple [C.Var "i1"; C.Var "b1"; C.IntLiteral 42]

            // (true or false, b1)
            u.Tuple [u.Or (u.Bool true, u.Bool false); u.Var "b1"], 
                Type.Tuple [Type.Bool; Type.Bool], 
                C.Tuple [C.Or (C.BoolLiteral true, C.BoolLiteral false); C.Var "b1"]


            // TODO: JOIN expressions

        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            u.Tuple [u.Var "foo"], "Unknown variable: foo"
            u.Tuple [u.Var "i1"; u.Var "b1"; u.Var "m1"], "Invalid tuple element. Expected bool or int expression, got: (Var \"m1\":Method ([], Type Int))"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestSet () =
        let ctx = this.TypeContext

        [
            // []
            u.Set [], Type.Set (Type.Tuple []), C.Set []
            // [3]
            u.Set [u.Int 3], Type.Set (Type.Int), C.Set [C.IntLiteral 3]
            // [3,5]
            u.Set [u.Int 3; u.Int 5], 
                Type.Set (Type.Int),
                C.Set [C.IntLiteral 3; C.IntLiteral 5]
            // [false, true, false, false]
            u.Set [u.Bool false; u.Bool true; u.Bool false; u.Bool false], 
                Type.Set (Type.Bool),
                C.Set [C.BoolLiteral false; C.BoolLiteral true; C.BoolLiteral false; C.BoolLiteral false]
            // [(3,5)]
            u.Set [u.Tuple [u.Int 3; u.Int 5]], 
                Type.Set (Type.Tuple [Type.Int; Type.Int]),
                C.Set [C.Tuple [C.IntLiteral 3; C.IntLiteral 5]]
            // [(f, b1, 4), (b1, true and false, 42)]
            u.Set [
                    u.Tuple [u.Bool false; u.Var "b1"; u.Int 4]
                    u.Tuple [u.Var "b1"; u.And (u.Bool true, u.Bool false); u.Int 42]],
                Type.Set (Type.Tuple [Type.Bool; Type.Bool; Type.Int]), 
                C.Set [
                    C.Tuple [C.BoolLiteral false; C.Var "b1"; C.IntLiteral 4]
                    C.Tuple [C.Var "b1"; C.And (C.BoolLiteral true, C.BoolLiteral false); C.IntLiteral 42]]
            // [t1, (true, 5)]
            u.Set [u.Var "t1"; u.Tuple [u.Bool true; u.Int 5]], 
                Type.Set (Type.Tuple [Type.Bool; Type.Int]),
                C.Set [C.Var "t1"; C.Tuple [C.BoolLiteral true; C.IntLiteral 5]]
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            u.Set [u.Var "foo"], "Unknown variable: foo"
            u.Set [u.Var "i1"; u.Var "b1"; u.Var "m1"], "Invalid set element. Expected int, bool or tuple expression, got: (Var \"m1\":Method ([], Type Int))"
            u.Set [u.Int 4; u.Bool true], "All elements in a set must be of the same type."
            u.Set [u.Tuple [u.Int 4; u.Bool true]; u.Tuple [u.Int 1; u.Int 2]], "All elements in a set must be of the same type."
            u.Set [u.Var "t1"; u.Tuple [u.Bool true; u.Int 5; u.Int 2]], "All elements in a set must be of the same type."
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestAndOrNot () =
        let ctx = this.TypeContext

        [
            // (not true)
            u.Not (u.Bool true), Type.Bool, C.Not (C.BoolLiteral true)
            // (true or false) and (b1)
            u.And (u.Or (u.Bool true, u.Bool false), u.Var "b1"),
                Type.Bool,
                C.And (C.Or (C.BoolLiteral true, C.BoolLiteral false), C.Var "b1")
            // (not (b1 or false))
            u.Not (u.Or (u.Var "b1", u.Bool false)), 
                Type.Bool, 
                C.Not (C.Or (C.Var "b1", C.BoolLiteral false))
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // (not | true >)
            u.Not (u.Ket [u.Bool true]), 
                QType.Ket [Type.Bool], 
                Q.Not  (Q.Literal (Set [C.BoolLiteral true]))

            // (qb1 or k2.1) and not | true >
            u.And (u.Or (u.Var "qb1", u.Project (u.Var "k2", [u.Int 1])), u.Not (u.Ket [u.Bool true])),
                QType.Ket [Type.Bool],
                Q.And (Q.Or (Q.Var "qb1", Q.Project (Q.Var "k2", [1])), (Q.Not (Q.Literal (Set [C.BoolLiteral true]))))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            u.And (u.Var "foo", u.Bool true), "Unknown variable: foo"
            u.And (u.Bool true, u.Int 23), "Boolean expressions require boolean arguments, got Bool == Int"
            u.Or (u.Bool true, u.Int 23), "Boolean expressions require boolean arguments, got Bool == Int"
            u.Not (u.Int 23), "Not must be applied to a boolean expression, got: Int"
        ]
        |> List.iter (this.TestInvalidExpression ctx)



    [<TestMethod>]
    member this.TestRange() =
        let ctx = this.TypeContext

        [
            // 0..0
            u.Range (u.Int 0, u.Int 0), 
                Type.Set Type.Int,
                C.Range (C.IntLiteral 0, C.IntLiteral 0)
            // 0..3 -> [0, 1, 2]
            u.Range (u.Var "i1", u.Int 0), 
                Type.Set Type.Int,
                C.Range (C.Var "i1", C.IntLiteral 0)
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // t1 .. 10: Invalid start type
            u.Range (u.Var "t1", u.Int 0), "Start must be an int expression, got: Classic (Var \"t1\", Tuple [Bool; Int])"
            // 10 .. t1: Invalid start type
            u.Range (u.Int 10 , u.Var "t1"), "Stop must be an int expression, got: Classic (Var \"t1\", Tuple [Bool; Int])"
        ]
        |> List.iter (this.TestInvalidExpression ctx)



    [<TestMethod>]
    member this.TestMethod() =
        let ctx = this.TypeContext

        [
            // let m () = true
            u.Method ([], u.Bool true),
                Type.Method ([], Type Type.Bool),
                C.Method ([], Classic (C.BoolLiteral true, Type.Bool))
            // let m (a: Int; b: Tuple<Int, Bool>) = b
            u.Method ([("a", Type Type.Int); ("b", Type (Type.Tuple [Type.Int; Type.Bool]))], u.Var "b"),
                Type.Method (
                    [Type Type.Int; Type (Type.Tuple [Type.Int; Type.Bool])], 
                    Type (Type.Tuple [Type.Int; Type.Bool])),
                C.Method (["a"; "b"], Classic (C.Var "b",  (Type.Tuple [Type.Int; Type.Bool])))
            // let m (i:Int) = 
            //      lambda (y: Bool) = 42
            u.Method ([("i", Type Type.Int)], u.Method (["y", Type Type.Bool], u.Int 42)),
                Type.Method (
                    [Type Type.Int], 
                    Type (Type.Method ([Type Type.Bool], Type Type.Int))),
                C.Method (["i"], (Classic (C.Method (
                    ["y"], 
                    Classic (C.IntLiteral 42, Type.Int)), Type.Method ([Type Type.Bool], Type Type.Int))))
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestKet() =
        let ctx = this.TypeContext

        [
            // |>
            u.Ket [],
                QType.Ket [],
                Q.Literal (C.Set [])
            // |1>
            u.Ket [u.Int 1],
                QType.Ket [Type.Int],
                Q.Literal (C.Set [C.IntLiteral 1])
            // |true, false>
            u.Ket [u.Bool true; u.Bool false],
                QType.Ket [Type.Bool],
                Q.Literal (C.Set [C.BoolLiteral true; C.BoolLiteral false])
            // |(1,2), (3,4)>
            u.Ket [u.Tuple [u.Int 1; u.Int 2]; u.Tuple [u.Int 3; u.Int 4]],
                QType.Ket [Type.Int; Type.Int],
                Q.Literal (C.Set [C.Tuple [C.IntLiteral 1; C.IntLiteral 2]; C.Tuple [C.IntLiteral 3; C.IntLiteral 4]])
            // |(1,false), (3,true)>
            u.Ket [u.Tuple [u.Int 1; u.Bool false]; u.Tuple [u.Int 3; u.Bool true]],
                QType.Ket [Type.Int; Type.Bool],
                Q.Literal (C.Set [C.Tuple [C.IntLiteral 1; C.BoolLiteral false]; C.Tuple [C.IntLiteral 3; C.BoolLiteral true]])
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            u.Ket [u.Bool true; u.Int 12], "All elements in a set must be of the same type."
            u.Ket [u.Tuple [u.Int 1; u.Bool false]; u.Tuple [u.Int 3; u.Int 4]], "All elements in a set must be of the same type."
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestKetAll() =
        let ctx = this.TypeContext

        [
            // |@,3>
            u.KetAll (u.Int 3),
                QInt,
                Q.KetAll (C.IntLiteral 3)
            // |@,i1>
            u.KetAll (u.Var "i1"),
                QInt,
                Q.KetAll (C.Var "i1")
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            u.KetAll (u.Bool false), "Ket size must be an int expression, got: Classic (BoolLiteral false, Bool)"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestClassicAdd() =
        let ctx = this.TypeContext

        [
            // 1 + 1
            u.Add (u.Int 1, u.Int 1),
                Type.Int,
                C.Add (C.IntLiteral 1, C.IntLiteral 1)
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // No overloading:
            u.Add (u.Bool true, u.Bool false), "Add can only be applied to int expressions"
            u.Add (u.Int 1, u.Bool false), "Add can only be applied to int expressions"
            u.Add (u.Tuple [u.Int 1], u.Tuple [u.Int 1]), "Add can only be applied to int expressions"
        ]
        |> List.iter (this.TestInvalidExpression ctx)

    [<TestMethod>]
    member this.TestQuantumAdd() =
        let ctx = this.TypeContext

        [
            // |0, 1> + |1, 2, 3>
            u.Add (u.Ket [u.Int 0;u.Int 1], u.Ket [u.Int 1; u.Int 2; u.Int 3]),
                QType.Ket [Type.Int],
                Q.Add (
                    Q.Literal (C.Set [C.IntLiteral 0; C.IntLiteral 1]), 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]))
            // 1 + |1, 2, 3>
            u.Add (u.Int 1, u.Ket [u.Int 1;u.Int 2;u.Int 3]),
                QType.Ket [Type.Int],
                Q.Add (
                    Q.Literal (C.Set [C.IntLiteral 1]), 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]))
            // |1, 2, 3> + 1
            u.Add (u.Ket [u.Int 1;u.Int 2;u.Int 3], u.Int 1),
                QType.Ket [Type.Int],
                Q.Add (
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]),
                    Q.Literal (C.Set [C.IntLiteral 1]))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            u.Add (u.Ket [u.Bool true; u.Int 1], u.Ket [u.Bool false; u.Int 2; u.Int 3]), "All elements in a set must be of the same type."
            u.Add (u.Ket [u.Bool true], u.Ket [u.Bool false]), "Quantum addition can only be applied to int Kets, got Ket [Bool] + Ket [Bool]"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestClassicEquals() =
        let ctx = this.TypeContext

        [
            // 1 + 1
            u.Equals (u.Int 1, u.Int 1),
                Type.Bool,
                C.Equals (C.IntLiteral 1, C.IntLiteral 1)
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // No overloading:
            u.Equals (u.Bool true, u.Bool false), "== can only be applied to int expressions"
            u.Equals (u.Int 1, u.Bool false), "== can only be applied to int expressions"
            u.Equals (u.Tuple [u.Int 1], u.Tuple [u.Int 1]), "== can only be applied to int expressions"
        ]
        |> List.iter (this.TestInvalidExpression ctx)

    [<TestMethod>]
    member this.TestQuantumEquals() =
        let ctx = this.TypeContext

        [
            // |0, 1> = |1, 2, 3>
            u.Equals (u.Ket [u.Int 0;u.Int 1], u.Ket [u.Int 1; u.Int 2; u.Int 3]),
                QType.Ket [Type.Bool],
                Q.Equals (
                    Q.Literal (C.Set [C.IntLiteral 0; C.IntLiteral 1]), 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]))
            // 1 = |1, 2, 3>
            u.Equals (u.Int 1, u.Ket [u.Int 1;u.Int 2;u.Int 3]),
                QType.Ket [Type.Bool],
                Q.Equals (
                    Q.Literal (C.Set [C.IntLiteral 1]), 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]))
            // |1, 2, 3> = 1
            u.Equals (u.Ket [u.Int 1;u.Int 2;u.Int 3], u.Int 1),
                QType.Ket [Type.Bool],
                Q.Equals (
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]),
                    Q.Literal (C.Set [C.IntLiteral 1]))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            u.Equals (u.Ket [u.Bool true; u.Int 1], u.Ket [u.Bool false; u.Int 2; u.Int 3]), "All elements in a set must be of the same type."
            u.Equals (u.Ket [u.Bool true], u.Ket [u.Bool false]), "Quantum == can only be applied to int Kets"
        ]
        |> List.iter (this.TestInvalidExpression ctx)

    [<TestMethod>]
    member this.TestClassicMultiply() =
        let ctx = this.TypeContext

        [
            // 1 * 1
            u.Multiply (u.Int 1, u.Int 1),
                Type.Int,
                C.Multiply (C.IntLiteral 1, C.IntLiteral 1)
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // No overloading:
            u.Multiply (u.Bool true, u.Bool false), "Multiply can only be applied to int expressions"
            u.Multiply (u.Int 1, u.Bool false), "Multiply can only be applied to int expressions"
            u.Multiply (u.Tuple [u.Int 1], u.Tuple [u.Int 1]), "Multiply can only be applied to int expressions"
        ]
        |> List.iter (this.TestInvalidExpression ctx)

    [<TestMethod>]
    member this.TestQuantumMultiply() =
        let ctx = this.TypeContext

        [
            // |0, 1> * |1, 2, 3>
            u.Multiply (u.Ket [u.Int 0;u.Int 1], u.Ket [u.Int 1; u.Int 2; u.Int 3]),
                QType.Ket [Type.Int],
                Q.Multiply (
                    Q.Literal (C.Set [C.IntLiteral 0; C.IntLiteral 1]), 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]))
            // 1 * |1, 2, 3>
            u.Multiply (u.Int 1, u.Ket [u.Int 1;u.Int 2;u.Int 3]),
                QType.Ket [Type.Int],
                Q.Multiply (
                    Q.Literal (C.Set [C.IntLiteral 1]), 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]))
            // |1, 2, 3> * 1
            u.Multiply (u.Ket [u.Int 1;u.Int 2;u.Int 3], u.Int 1),
                QType.Ket [Type.Int],
                Q.Multiply (
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]),
                    Q.Literal (C.Set [C.IntLiteral 1]))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            u.Multiply (u.Ket [u.Bool true; u.Int 1], u.Ket [u.Bool false; u.Int 2; u.Int 3]), "All elements in a set must be of the same type."
            u.Multiply (u.Ket [u.Bool true], u.Ket [u.Bool false]), "Quantum multiplication can only be applied to int Kets"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestLessThan() =
        let ctx = this.TypeContext

        [
            // 1 < 1
            u.LessThan (u.Int 1, u.Int 1),
                Type.Bool,
                C.LessThan (C.IntLiteral 1, C.IntLiteral 1)
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            u.LessThan (u.Bool true, u.Bool false), "Both expressions for < must be int. Got Bool < Bool"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestCallMethod() =
        let ctx = 
            this.TypeContext
                .Add("m2", AnyType.Type (
                    Type.Method (
                        [AnyType.QType (QType.Ket [Type.Int]); AnyType.Type (Type.Tuple [Type.Int; Type.Bool])], 
                        AnyType.Type (Type.Tuple [Type.Int; Type.Int]))))
                .Add("q2", AnyType.Type (
                    Type.Method (
                        [AnyType.QType (QType.Ket [Type.Int]); AnyType.Type (Type.Tuple [Type.Int; Type.Bool])], 
                        AnyType.QType (QType.Ket [Type.Bool]))))

        [
            // m1()
            u.CallMethod (u.Var "m1", []),
                Type.Int,
                C.CallMethod (C.Var ("m1"), [])
            // m2(Ket<Int>, Tuple<Int, Bool>) : Tuple<Int, Int>
            u.CallMethod (u.Var "m2", [u.Ket [u.Int 1]; u.Tuple [u.Int 2; u.Bool false]]),
                Type.Tuple [Type.Int; Type.Int],
                C.CallMethod (C.Var ("m2"),  [
                    Quantum (Q.Literal (C.Set [C.IntLiteral 1]), QType.Ket [Type.Int]); 
                    Classic (C.Tuple [C.IntLiteral 2; C.BoolLiteral false], Type.Tuple[Type.Int; Type.Bool])])
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // q1()
            u.CallMethod (u.Var "q1", []),
                QType.Ket [Type.Int],
                Q.CallMethod (C.Var ("q1"), [])
            // m2(Ket<Int>, (Int, Bool) : Ket<Bool>
            u.CallMethod (u.Var "q2", [u.Ket [u.Int 1]; u.Tuple [u.Int 2; u.Bool false]]),
                QType.Ket [Type.Bool],
                Q.CallMethod (C.Var ("q2"),  [
                    Quantum (Q.Literal (C.Set [C.IntLiteral 1]), QType.Ket[Type.Int]); 
                    Classic (C.Tuple [C.IntLiteral 2; C.BoolLiteral false], Type.Tuple[Type.Int; Type.Bool])])
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            u.CallMethod (u.Var "m1", [u.Int 1]), "Arguments type missmatch. Expected [] got [Type Int]"
            u.CallMethod (u.Var "q1", [u.Int 1]), "Arguments type missmatch. Expected [] got [Type Int]"
            u.CallMethod (u.Var "m2", [u.Int 1; u.Tuple [u.Int 2; u.Bool false]]), "Arguments type missmatch. Expected [QType (Ket [Int]); Type (Tuple [Int; Bool])] got [Type Int; Type (Tuple [Int; Bool])]"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestJoin() =
        let ctx = this.TypeContext

        [
            // ((), ())
            u.Join (u.Tuple [], u.Tuple []),
                Type.Tuple [],
                C.Join (C.Tuple [], C.Tuple [])
            // (t1, ())
            u.Join (u.Var "t1", u.Tuple []),
                Type.Tuple [Type.Bool; Type.Int],
                C.Join (C.Var "t1", C.Tuple [])
            // ((1,1), (0,0,false))
            u.Join (u.Tuple [u.Int 1; u.Int 1], u.Tuple [u.Int 0; u.Int 0; u.Bool true]),
                Type.Tuple [Type.Int; Type.Int; Type.Int; Type.Int; Type.Bool],
                C.Join (C.Tuple [C.IntLiteral 1; C.IntLiteral 1], C.Tuple[C.IntLiteral 0; C.IntLiteral 0; C.BoolLiteral true])
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // (|>|), |>)
            u.Join (u.Ket [], u.Ket []),
                QType.Ket [],
                Q.Join (Q.Literal (C.Set []), Q.Literal (C.Set []))
            // (t1, |>)
            u.Join (u.Var "k1", u.Ket []),
                QType.Ket [Type.Int],
                Q.Join (Q.Var "k1", Q.Literal (C.Set []))
            // (t1, |1,2,3>)
            u.Join (u.Var "k1", u.Ket [u.Int 1; u.Int 2; u.Int 3]),
                QType.Ket [Type.Int; Type.Int],
                Q.Join (
                    Q.Var "k1", 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]))
            // (|(1,1)>, |(0,0,false)>)
            u.Join (u.Ket [u.Tuple [u.Int 1; u.Int 1]], u.Ket [u.Tuple [u.Int 0; u.Int 0; u.Bool true]]),
                QType.Ket [Type.Int; Type.Int; Type.Int; Type.Int; Type.Bool],
                Q.Join (
                    Q.Literal (C.Set [C.Tuple [C.IntLiteral 1; C.IntLiteral 1]]),
                    Q.Literal (C.Set [C.Tuple [C.IntLiteral 0; C.IntLiteral 0; C.BoolLiteral true]]))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            u.Join (u.Bool true, u.Int 1), "Join is only supported on tuples and kets, got: Classic (BoolLiteral true, Bool) , Classic (IntLiteral 1, Int)"
        ]
        |> List.iter (this.TestInvalidExpression ctx)



    [<TestMethod>]
    member this.TestProject() =
        let ctx = this.TypeContext

        [
            // (1).0
            u.Project (u.Tuple [Int 1], [Int 0]),
                Type.Int,
                C.Project (C.Tuple [C.IntLiteral 1], [0])
            // (1).[0, 2]  --> Note, index in projection is modular, so 2 == 0
            u.Project (u.Tuple [Int 1], [Int 0; Int 0]),
                Type.Tuple [Type.Int; Type.Int],
                C.Project (C.Tuple [C.IntLiteral 1], [0; 0])
            // (1, false, 3, true).3
            u.Project (u.Tuple [Int 1; u.Bool false; Int 3; u.Bool true], [Int 3]),
                Type.Bool,
                C.Project (C.Tuple [C.IntLiteral 1; C.BoolLiteral false; C.IntLiteral 3; C.BoolLiteral true], [3])
            // (1, false, 3, true).[1,2]
            u.Project (u.Tuple [Int 1; u.Bool false; Int 3; u.Bool true], [Int 1; Int 2]),
                Type.Tuple [Type.Bool; Type.Int],
                C.Project (C.Tuple [C.IntLiteral 1; C.BoolLiteral false; C.IntLiteral 3; C.BoolLiteral true], [1;2])
            // (1).[i1, i1, 0]
            u.Project (u.Tuple [Int 1], [u.Var "i1"; u.Var "i1"; u.Int 0]),
                Type.Tuple [Type.Int; Type.Int; Type.Int],
                C.Index (C.Tuple [C.IntLiteral 1], [C.Var "i1";C.Var "i1";C.IntLiteral 0])
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // |1>.0
            u.Project (u.Ket [Int 1], [Int 0]),
                QType.Ket [Type.Int],
                Q.Project (Q.Literal (C.Set [C.IntLiteral 1]), [0])
            // k1.[0, 2]  --> Note, index in projection is modular, so 2 == 0
            u.Project (u.Var "k1", [Int 0; Int 0]),
                QType.Ket [Type.Int; Type.Int],
                Q.Project (Q.Var "k1", [0; 0])
            // k2.1
            u.Project (u.Var "k2", [Int 1]),
                QType.Ket [Type.Bool],
                Q.Project (Q.Var "k2", [1])
            // |(1, false, 3, true)>.[1,2]
            u.Project (u.Ket [u.Tuple [Int 1; u.Bool false; Int 3; u.Bool true]], [Int 1; Int 2]),
                QType.Ket [Type.Bool; Type.Int],
                Q.Project (Q.Literal (C.Set [Tuple [C.IntLiteral 1; C.BoolLiteral false; C.IntLiteral 3; C.BoolLiteral true]]), [1;2])
            // k1.[i1, i1, 0]
            u.Project (u.Var "k1", [u.Var "i1"; u.Var "i1"; u.Int 0]),
                QType.Ket [Type.Int; Type.Int; Type.Int],
                Q.Index (Q.Var "k1", [C.Var "i1"; C.Var "i1"; C.IntLiteral 0])
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            // (1, false, 3).[i1,2]
            u.Project (u.Tuple [Int 1; u.Bool false; Int 3], [u.Var "i1"; Int 2]), "Indexing of tuples is only available on tuples of a single type"
            // |(1, false, 3)>.[i1,2]
            u.Project (u.Ket [u.Tuple [Int 1; u.Bool false; Int 3]], [u.Var "i1"; Int 2]), "Indexing of kets is only available on kets of a single type"
            // [(1, false, 3)].[0]
            u.Project (u.Set [u.Tuple [Int 1; u.Bool false; Int 3]], [u.Int 2]), "Project is only supported on tuples and kets"
            // [(1, false, 3)].[i1]
            u.Project (u.Set [u.Tuple [Int 1; u.Bool false; Int 3]], [u.Var "i1"]), "Project is only supported on tuples and kets"
            // [1, 2, 3].[false]
            u.Project (u.Set [Int 1; u.Int 2; Int 3], [u.Bool false]), "Invalid projection index. Expected int expression, got: (BoolLiteral false:Bool)"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestBlock() =
        let ctx = this.TypeContext

        [
            // { t1 }
            u.Block ([], (u.Var "t1")),
                Type.Tuple [Type.Bool; Type.Int],
                C.Block ([], C.Var "t1")
            // { let a = 15; print "some msg" a k1; a + i1 }
            u.Block ([
                aleph.parser.ast.Statement.Let ("a", u.Int 15)
                aleph.parser.ast.Statement.Print ("some msg", [u.Var "a"; u.Var "k1"])],
                (u.Add (u.Var "a", u.Var "i1"))),
                Type.Int,
                C.Block ([
                    Let ("a", Classic (C.IntLiteral 15, Type.Int))
                    Print ("some msg", [Classic (C.Var "a", Type.Int); (Quantum (Q.Var "k1", QType.Ket [Type.Int]))])],
                    C.Add (C.Var "a", C.Var "i1"))
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // { t1 }
            u.Block ([], (u.Var "k1")),
                QType.Ket [Type.Int],
                Q.Block ([], Q.Var "k1")
            // { let a = 15; print "some msg" a k1; a + k1 }
            u.Block ([
                aleph.parser.ast.Statement.Let ("a", u.Int 15)
                aleph.parser.ast.Statement.Print ("some msg", [u.Var "a"; u.Var "k1"])],
                (u.Add (u.Var "a", u.Var "k1"))),
                QType.Ket [Type.Int],
                Q.Block ([
                    Let ("a", Classic (C.IntLiteral 15, Type.Int))
                    Print ("some msg", [Classic (C.Var "a", Type.Int); (Quantum (Q.Var "k1", QType.Ket [Type.Int]))])],
                    Q.Add (Q.Literal (C.Set [C.Var "a"]), Q.Var "k1"))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            // { let s = 5 == false; a}
            u.Block ([aleph.parser.ast.Statement.Let ("a", u.Equals (u.Int 5, u.Bool false))], u.Var "a"), "== can only be applied to int expressions"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestIf() =
        let ctx = this.TypeContext

        [
            // { if true then 1 else 0 }
            u.If (u.Bool true, u.Int 1, u.Int 0),
                Type.Int,
                C.If (C.BoolLiteral true, C.IntLiteral 1, C.IntLiteral 0)
            // { if b1 or t1.0 then (0, 1) else (0, 0) }
            u.If (u.Or (u.Var "b1", u.Project (u.Var "t1", [u.Int 0])), u.Tuple [u.Int 0; u.Int 1], u.Tuple [u.Int 0; u.Int 0]),
                Type.Tuple [Type.Int; Type.Int],
                C.If (C.Or (C.Var "b1", C.Project (C.Var "t1", [0])), C.Tuple [C.IntLiteral 0; C.IntLiteral 1], C.Tuple [C.IntLiteral 0; C.IntLiteral 0])
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // { if true then k1 else |0> }
            u.If (u.Bool true, u.Var "k1", u.Ket [u.Int 0]),
                QType.Ket [Type.Int],
                Q.IfClassic (C.BoolLiteral true, Q.Var "k1", Q.Literal (Set [C.IntLiteral 0]))
            // { if true then k1 else 0 }
            u.If (u.Bool true, u.Var "k1", u.Int 0),
                QType.Ket [Type.Int],
                Q.IfClassic (C.BoolLiteral true, Q.Var "k1", Q.Literal (Set [C.IntLiteral 0]))
            // { if k2.1 then (0, 1) else (0, 0) }
            u.If (u.Project (u.Var "k2", [u.Int 1]), u.Tuple [u.Int 0; u.Int 1], u.Tuple [u.Int 0; u.Int 0]),
                QType.Ket [Type.Int; Type.Int],
                Q.IfQuantum (
                    Q.Project (Q.Var "k2", [1]), 
                    Q.Literal (C.Set [C.Tuple [C.IntLiteral 0; C.IntLiteral 1]]), 
                    Q.Literal (C.Set [C.Tuple [C.IntLiteral 0; C.IntLiteral 0]]))
            // TODO: QUANTUM OR
            // // { if b1 or k1.1 then (0, 1) else (0, 0) }
            // u.If (u.Or [u.Var "b1"; u.Project (u.Var "k1", [u.Int 1])], u.Tuple [u.Int 0; u.Int 1], u.Tuple [u.Int 0; u.Int 0]),
            //     QType.Ket [Type.Int; Type.Int],
            //     Q.IfQuantum (
            //         Q.Or (Q.Join (Q.Literal (C.Var "b1"), Q.Project (Q.Var "k1", [1]))), 
            //         Q.Literal (C.Tuple [C.IntLiteral 0; C.IntLiteral 1]), 
            //         Q.Literal (C.Tuple [C.IntLiteral 0; C.IntLiteral 0]))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            // { if true then 1 else false }
            u.If (u.Bool true, u.Int 1, u.Bool false), "Both branches of if statement must be of the same type, got Int and Bool"
            // { if true then k1 else k2 }
            u.If (u.Bool true, u.Var "k1", u.Var "k2"), "Both branches of if statement must be of the same type, got Ket [Int] and Ket [Int; Bool]"
            // { if 42 then 1 else 2 }
            u.If (u.Int 42, u.Int 1, u.Int 2), "If condition must be a boolean, got Int"
            // { if 42 then |1> else |0> }
            u.If (u.Int 42, u.Ket [u.Int 1], u.Ket [u.Int 0]), "If condition must be a boolean, got Int"
        ]
        |> List.iter (this.TestInvalidExpression ctx)



    [<TestMethod>]
    member this.TestSummarize() =
        let ctx = this.TypeContext

        [
            // summarize e in s1 with and { e.1 < 10 } 
            u.Summarize ("e", u.Var "s1", Aggregation.And, u.LessThan (u.Project (u.Var "e", [u.Int 1]), u.Int 10)),
                Type.Bool,
                C.Summarize ("e", C.Var "s1", Aggregation.And, C.LessThan (C.Project (C.Var "e", [1]), C.IntLiteral 10))
            // summarize e in s1 with or { e.1 == 10 } 
            u.Summarize ("e", u.Var "s1", Aggregation.Or, u.Equals (u.Project (u.Var "e", [u.Int 1]), u.Int 10)),
                Type.Bool,
                C.Summarize ("e", C.Var "s1", Aggregation.Or, C.Equals (C.Project (C.Var "e", [1]), C.IntLiteral 10))
            // summarize e in [true, false] with or { true } 
            u.Summarize ("e", u.Set [u.Bool true; u.Bool false], Aggregation.Or, u.Bool true),
                Type.Bool,
                C.Summarize ("e", C.Set [C.BoolLiteral true; C.BoolLiteral false], Aggregation.Or, C.BoolLiteral true)
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // summarize e in s1 with and { k2.0 == e.1 } 
            u.Summarize ("e", u.Var "s1", Aggregation.And, u.Equals (u.Project (u.Var "k2", [u.Int 0]), u.Project (u.Var "e", [u.Int 1]))),
                QType.Ket [Type.Bool],
                Q.Summarize ("e", C.Var "s1", Aggregation.And, Q.Equals (Q.Project (Q.Var "k2", [0]), (Q.Literal (C.Set [C.Project (C.Var "e", [1])]))))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            // summarize e in s1 with and { e < 10 } 
            u.Summarize ("e", u.Var "s1", Aggregation.And, u.LessThan ((u.Var "e"), u.Int 10)), "Both expressions for < must be int. Got Tuple [Bool; Int] < Int"
            // summarize e in t1 with and { e.0 == true } 
            u.Summarize ("e", u.Var "t1", Aggregation.And, u.Equals (u.Project (u.Var "e", [u.Int 0]), u.Bool true)), "Summarize expects a classic set of values, got: Tuple [Bool; Int]"
            // summarize e in k1 with and { e.0 == |1> } 
            u.Summarize ("e", u.Var "k1", Aggregation.And, u.Equals (u.Project (u.Var "e", [u.Int 0]), u.Ket [u.Int 1])), "Summarize expects a classic set of values, got: Ket [Int]"
            // summarize e in s1 with and { k2.1 == e.1 } 
            u.Summarize ("e", u.Var "s1", Aggregation.And, u.Equals (u.Project (u.Var "k2", [u.Int 1]), u.Project (u.Var "e", [u.Int 1]))), "Quantum == can only be applied to int Kets"
            // summarize e in s1 with sum { true } 
            u.Summarize ("e", u.Var "s1", Aggregation.Sum, u.Bool true), "Summarize body must be an Int expression when aggregation is 'sum', got Bool"
            // summarize e in s1 with and { 1 } 
            u.Summarize ("e", u.Var "s1", Aggregation.And, u.Int 1), "Summarize body must be a boolean expression when aggregation is 'and' | 'or', got Int"
            // summarize e in s1 with or { 1 } 
            u.Summarize ("e", u.Var "s1", Aggregation.Or, u.Int 1), "Summarize body must be a boolean expression when aggregation is 'and' | 'or', got Int"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestSolve() =
        let ctx = this.TypeContext

        [
            // k2 | k2.[0] == 1
            u.Solve(u.Var "k2", u.Equals (u.Project (u.Var "k2", [u.Int 0]), u.Int 1)),
                QType.Ket [Type.Int; Type.Bool],
                Q.Solve(Q.Var "k2", Q.Equals (Q.Project (Q.Var "k2", [0]), Q.Literal (C.Set [C.IntLiteral 1])))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            u.Solve (u.Tuple [u.Int 1;u.Int 2], u.Equals (u.Project (u.Var "k2", [u.Int 1]), u.Bool true)), "Solve argument must be a quantum ket, got: Tuple [Int; Int]"
            u.Solve (u.Var "k2", u.Bool true), "Solve condition must be a quantum boolean expression, got: Bool"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestSample() =
        let ctx = this.TypeContext

        [
            // | |> |
            u.Sample (u.Ket []),
                Type.Tuple [],
                C.Sample(Q.Literal (C.Set []))
            // | k1 |
            u.Sample (u.Var "k1"),
                Type.Tuple [Type.Int],
                C.Sample(Q.Var "k1")
            // | k2 |
            u.Sample (u.Var "k2"),
                Type.Tuple [Type.Int; Type.Bool],
                C.Sample(Q.Var "k2")
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            u.Sample (u.Tuple [u.Int 1;u.Int 2]), "Sample argument must be a quantum ket, got: Tuple [Int; Int]"
        ]
        |> List.iter (this.TestInvalidExpression ctx)
