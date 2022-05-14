namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.parser.ast
open aleph.parser.typed
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
            "qb1", AnyType.QType (QType.QBool)
            "k1", AnyType.QType (QType.Ket [QType.QInt])
            "k2", AnyType.QType (QType.Ket [QType.QInt; QType.QBool])
            "m1", AnyType.Type (Type.CMethod ([], Type.Int))
            "qm1", AnyType.Type (Type.QMethod ([], QType.QBool, QType.QInt))
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
            u.Var "m1", Type.CMethod ([], Type.Int), C.Var "m1"
            u.Var "qm1", Type.QMethod ([], QType.QBool, QType.QInt), C.Var "qm1"
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            u.Var "qb1", QType.QBool, Q.Var "qb1"
            u.Var "k1", QType.Ket [QType.QInt], Q.Var "k1"
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
            u.Tuple [u.Var "i1"; u.Var "b1"; u.Var "m1"], "Invalid tuple element. Expected bool or int expression, got: (Var \"m1\":CMethod ([], Int))"
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
            u.Set [u.Var "i1"; u.Var "b1"; u.Var "m1"], "Invalid set element. Expected int, bool or tuple expression, got: (Var \"m1\":CMethod ([], Int))"
            u.Set [u.Int 4; u.Bool true], "All tuples in a set must be of the same type."
            u.Set [u.Tuple [u.Int 4; u.Bool true]; u.Tuple [u.Int 1; u.Int 2]], "All tuples in a set must be of the same type."
            u.Set [u.Var "t1"; u.Tuple [u.Bool true; u.Int 5; u.Int 2]], "All tuples in a set must be of the same type."
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
            u.And [u.Var "qb1"], "Invalid And element. Expected bool expression, got: (Var \"qb1\":QBool)"
            u.And [u.Bool true; u.Int 23], "Invalid And element. Expected bool expression, got: (IntLiteral 23:Int)"
            u.Or [u.Bool true; u.Int 23], "Invalid Or element. Expected bool expression, got: (IntLiteral 23:Int)"
            u.Not (u.Int 23), "Not expressions require boolean arguments, got: Classic (IntLiteral 23, Int)"
        ]
        |> List.iter (this.TestInvalidExpression ctx)
