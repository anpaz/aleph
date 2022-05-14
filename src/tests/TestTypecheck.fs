namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

// open aleph.runtime.Core
open aleph.parser.ast
open aleph.parser.typed
open aleph.parser.TypeChecker

type e = Expression

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


    member this.TypeContext =
        let i1 = AnyType.Type Type.Int
        let b1 = AnyType.Type Type.Bool
        let t1 = AnyType.Type (Type.Tuple [Type.Bool; Type.Int])
        let t2 = AnyType.Type (Type.Tuple [Type.Bool; Type.Int])
        let s1 = AnyType.Type (Type.Set (Type.Tuple [Type.Bool; Type.Int]))

        // classic = plusone (a) -> a + 1
        // let plusone = Value.Method (["a"], Add([Id "a"; Int 1]))

        TypeContext [ 
            ("i1", i1)
            ("b1", b1)
            ("t1", t1)
            ("t2", t2)
            ("s1", s1)
            // ("plusone", plusone)
        ]


    [<TestMethod>]
    member this.TestBoolInt () =
        let ctx = Map.empty

        [
            e.Bool false, Type.Bool, C.BoolLiteral false
            e.Int 5, Type.Int, C.IntLiteral 5
        ]
        |> List.iter (this.TestClassicExpression ctx)


    [<TestMethod>]
    member this.TestTuple () =
        let ctx = this.TypeContext

        [
            // () -> ()
            e.Tuple [], Type.Tuple [], C.Tuple []
            // (3) -> (3)
            e.Tuple [Expression.Int 3], Type.Tuple [Type.Int], C.Tuple [C.IntLiteral 3]
            // (3,5) -> (3,5)
            e.Tuple [e.Int 3; e.Int 5], Type.Tuple [Type.Int; Type.Int], C.Tuple [C.IntLiteral 3; C.IntLiteral 5]
            // (f, b1, t) -> (f, f, t)
            e.Tuple [e.Bool false; e.Var "b1"; e.Bool true], 
                Type.Tuple [Type.Bool; Type.Bool; Type.Bool], 
                C.Tuple [C.BoolLiteral false; C.Var "b1"; C.BoolLiteral true]
            // (i1, i1) -> (3, 3)
            e.Tuple [e.Var "i1"; e.Var "i1"], 
                Type.Tuple [Type.Int; Type.Int], 
                C.Tuple [C.Var "i1"; C.Var "i1"]
            // // ((3)) --> (3)
            // (e.Tuple([e.Tuple([Int(3)])]), Value.Tuple([I(3)]))
            // // ((0,1)) --> (0,1)
            // (Tuple([Tuple([Int(0); Int(1)])]), Value.Tuple([I(0); I(1)]))
        ]
        |> List.iter (this.TestClassicExpression ctx)