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
            "m1", AnyType.Type (Type.Method ([], Type.Int))
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
            u.Var "m1", Type.Method ([], Type.Int), C.Var "m1"
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
            u.Tuple [u.Or [u.Bool true; u.Bool false]; u.Var "b1"], 
                Type.Tuple [Type.Bool; Type.Bool], 
                C.Tuple [C.Or [C.BoolLiteral true; C.BoolLiteral false]; C.Var "b1"]


            // TODO: JOIN expressions

        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            u.Tuple [u.Var "foo"], "Unknown variable: foo"
            u.Tuple [u.Var "i1"; u.Var "b1"; u.Var "m1"], "Invalid tuple element. Expected bool or int expression, got: (Var \"m1\":Method ([], Int))"
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
            // [(f, b1, 4), (b1, true and true and false, 42)]
            u.Set [
                    u.Tuple [u.Bool false; u.Var "b1"; u.Int 4]
                    u.Tuple [u.Var "b1"; u.And [u.Bool true; u.Bool true; u.Bool false]; u.Int 42]],
                Type.Set (Type.Tuple [Type.Bool; Type.Bool; Type.Int]), 
                C.Set [
                    C.Tuple [C.BoolLiteral false; C.Var "b1"; C.IntLiteral 4]
                    C.Tuple [C.Var "b1"; C.And [C.BoolLiteral true; C.BoolLiteral true; C.BoolLiteral false]; C.IntLiteral 42]]
            // [t1, (true, 5)]
            u.Set [u.Var "t1"; u.Tuple [u.Bool true; u.Int 5]], 
                Type.Set (Type.Tuple [Type.Bool; Type.Int]),
                C.Set [C.Var "t1"; C.Tuple [C.BoolLiteral true; C.IntLiteral 5]]
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            u.Set [u.Var "foo"], "Unknown variable: foo"
            u.Set [u.Var "i1"; u.Var "b1"; u.Var "m1"], "Invalid set element. Expected int, bool or tuple expression, got: (Var \"m1\":Method ([], Int))"
            u.Set [u.Int 4; u.Bool true], "All elements in a set must be of the same type."
            u.Set [u.Tuple [u.Int 4; u.Bool true]; u.Tuple [u.Int 1; u.Int 2]], "All elements in a set must be of the same type."
            u.Set [u.Var "t1"; u.Tuple [u.Bool true; u.Int 5; u.Int 2]], "All elements in a set must be of the same type."
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestAndOrNot () =
        let ctx = this.TypeContext

        [
            // Typechecks, but it should probably fail eval:
            u.And [], Type.Bool, C.And []
            u.Or [], Type.Bool, C.Or []
            // (true)
            u.And [u.Bool true], Type.Bool, C.And [C.BoolLiteral true]
            // (not true)
            u.Not (u.Bool true), Type.Bool, C.Not (C.BoolLiteral true)
            // (true or false or false) and (b1)
            u.And [u.Or [u.Bool true; u.Bool false; u.Bool false]; u.Var "b1"],
                Type.Bool,
                C.And [C.Or [C.BoolLiteral true; C.BoolLiteral false; C.BoolLiteral false]; C.Var "b1"]
            // (not (b1 or false))
            u.Not (u.Or [u.Var "b1"; u.Bool false]), 
                Type.Bool, 
                C.Not (C.Or [C.Var "b1"; C.BoolLiteral false])
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            u.And [u.Var "foo"], "Unknown variable: foo"
            u.And [u.Var "qb1"], "Invalid And element. Expected bool expression, got: (Var \"qb1\":Ket [Bool])"
            u.And [u.Bool true; u.Int 23], "Invalid And element. Expected bool expression, got: (IntLiteral 23:Int)"
            u.Or [u.Bool true; u.Int 23], "Invalid Or element. Expected bool expression, got: (IntLiteral 23:Int)"
            u.Not (u.Int 23), "Not expressions require boolean arguments, got: Classic (IntLiteral 23, Int)"
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
                Type.Method ([], Type.Bool),
                C.Method ([], C.BoolLiteral true)
            // let m (a: Int; b: Tuple<Int, Bool>) = b
            u.Method ([("a", Type Type.Int); ("b", Type (Type.Tuple [Type.Int; Type.Bool]))], u.Var "b"),
                Type.Method ([Type Type.Int; Type (Type.Tuple [Type.Int; Type.Bool])], Type.Tuple [Type.Int; Type.Bool]),
                C.Method (["a"; "b"], C.Var "b")
            // let m (i:Int) = 
            //      lambda (y: Bool) = 42
            u.Method ([("i", Type Type.Int)], u.Method (["y", Type Type.Bool], u.Int 42)),
                Type.Method ([Type Type.Int], Type.Method ([Type Type.Bool], Type.Int)),
                C.Method (["i"], C.Method (["y"], C.IntLiteral 42))
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // let m () = |@,2>
            u.Method ([], u.KetAll (u.Int 2)), "Methods must have a classic return type"
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
                Q.Add (Q.Join (
                    Q.Literal (C.Set [C.IntLiteral 0; C.IntLiteral 1]), 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3])))
            // 1 + |1, 2, 3>
            u.Add (u.Int 1, u.Ket [u.Int 1;u.Int 2;u.Int 3]),
                QType.Ket [Type.Int],
                Q.Add (Q.Join (
                    Q.Literal (C.Set [C.IntLiteral 1]), 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3])))
            // |1, 2, 3> + 1
            u.Add (u.Ket [u.Int 1;u.Int 2;u.Int 3], u.Int 1),
                QType.Ket [Type.Int],
                Q.Add (Q.Join (
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]),
                    Q.Literal (C.Set [C.IntLiteral 1])))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            u.Add (u.Ket [u.Bool true; u.Int 1], u.Ket [u.Bool false; u.Int 2; u.Int 3]), "All elements in a set must be of the same type."
            u.Add (u.Ket [u.Bool true], u.Ket [u.Bool false]), "Quantum addition can only be applied to int Kets"
        ]
        |> List.iter (this.TestInvalidExpression ctx)
