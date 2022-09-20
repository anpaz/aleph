namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.parser.ast
open aleph.parser.ast.typed
open aleph.parser.TypeChecker


[<TestClass>]
type TestTypecheck () =

    member this.TestClassicExpression ctx (e, t, r)=
        match typecheck (e, ctx) with
        | Ok (Classic (r', t'), _) -> 
            Assert.AreEqual(r, r')
            Assert.AreEqual(t, t')
        | Ok (Quantum (r', t'), _) -> 
            Assert.AreEqual($"Classic {r}:{t}", $"Quantum {r'}: {t'}")
        | Ok (Universe (r', t'), _) -> 
            Assert.AreEqual($"Classic {r}:{t}", $"Quantum {r'}: {t'}")
        | Error msg -> 
            Assert.AreEqual($"Classic {r}:{t}", $"Error msg: {msg}")

    member this.TestQuantumExpression ctx (e, t, r)=
        printfn "e:%A" e
        match typecheck (e, ctx) with
        | Ok (Classic (r', t'), _) -> 
            Assert.AreEqual($"Quantum {r}:{t}", $"Classic {r'}: {t'}")
        | Ok (Quantum (r', t'), _) -> 
            Assert.AreEqual(r, r')
            Assert.AreEqual(t, t')
        | Ok (Universe (r', t'), _) -> 
            Assert.AreEqual($"Quantum {r}:{t}", $"Universe {r'}: {t'}")
        | Error msg -> 
            Assert.AreEqual($"Quantum {r}:{t}", $"Error msg: {msg}")

    member this.TestInvalidExpression ctx (e, error) =
        match typecheck (e, ctx) with
        | Ok (v, _) ->
            Assert.AreEqual($"Expected error: {error}", $"got {v}")
        | Error msg -> 
            Assert.AreEqual(error, msg)

    member this.TypeContext =
        { 
            heap = Map ([ 
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
            ])
            previousCtx = None
        }


    [<TestMethod>]
    member this.TestBoolInt () =
        let ctx = { heap = Map.empty; previousCtx = None }

        [
            e.Bool false, Type.Bool, C.BoolLiteral false
            e.Int 5, Type.Int, C.IntLiteral 5
        ]
        |> List.iter (this.TestClassicExpression ctx)


    [<TestMethod>]
    member this.TestVar () =
        let ctx = this.TypeContext

        [
            e.Var "i1", Type.Int, C.Var "i1"
            e.Var "m1", Type.Method ([], Type Type.Int), C.Var "m1"
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            e.Var "qb1", QBool, Q.Var "qb1"
            e.Var "k1", QInt, Q.Var "k1"
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            e.Var "foo", "Variable not found: foo"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestTuple () =
        let ctx = this.TypeContext

        [
            // ()
            e.Tuple [], Type.Tuple [], C.Tuple []
            // (3)
            e.Tuple [e.Int 3], Type.Tuple [Type.Int], C.Tuple [C.IntLiteral 3]
            // (3,5)
            e.Tuple [e.Int 3; e.Int 5], 
                Type.Tuple [Type.Int; Type.Int],
                C.Tuple [C.IntLiteral 3; C.IntLiteral 5]
            // (f, b1, t)
            e.Tuple [e.Bool false; e.Var "b1"; e.Bool true], 
                Type.Tuple [Type.Bool; Type.Bool; Type.Bool], 
                C.Tuple [C.BoolLiteral false; C.Var "b1"; C.BoolLiteral true]
            // (i1, i1)
            e.Tuple [e.Var "i1"; e.Var "i1"], 
                Type.Tuple [Type.Int; Type.Int], 
                C.Tuple [C.Var "i1"; C.Var "i1"]
            // (i1, b1, 42)
            e.Tuple [e.Var "i1"; e.Var "b1"; e.Int 42], 
                Type.Tuple [Type.Int; Type.Bool; Type.Int], 
                C.Tuple [C.Var "i1"; C.Var "b1"; C.IntLiteral 42]

            // (true or false, b1)
            e.Tuple [e.Or (e.Bool true, e.Bool false); e.Var "b1"], 
                Type.Tuple [Type.Bool; Type.Bool], 
                C.Tuple [C.Or (C.BoolLiteral true, C.BoolLiteral false); C.Var "b1"]

            // TODO: JOIN expressions

        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            e.Tuple [e.Var "foo"], "Variable not found: foo"
            e.Tuple [e.Var "i1"; e.Var "b1"; e.Var "m1"], "Invalid tuple element. Expected bool or int expression, got: Method ([], Type Int)"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestSet () =
        let ctx = this.TypeContext

        [
            // []
            e.Set [], Type.Set (Type.Tuple []), C.Set []
            // [3]
            e.Set [e.Int 3], Type.Set (Type.Int), C.Set [C.IntLiteral 3]
            // [3,5]
            e.Set [e.Int 3; e.Int 5], 
                Type.Set (Type.Int),
                C.Set [C.IntLiteral 3; C.IntLiteral 5]
            // [false, true, false, false]
            e.Set [e.Bool false; e.Bool true; e.Bool false; e.Bool false], 
                Type.Set (Type.Bool),
                C.Set [C.BoolLiteral false; C.BoolLiteral true; C.BoolLiteral false; C.BoolLiteral false]
            // [(3,5)]
            e.Set [e.Tuple [e.Int 3; e.Int 5]], 
                Type.Set (Type.Tuple [Type.Int; Type.Int]),
                C.Set [C.Tuple [C.IntLiteral 3; C.IntLiteral 5]]
            // [(f, b1, 4), (b1, true and false, 42)]
            e.Set [
                    e.Tuple [e.Bool false; e.Var "b1"; e.Int 4]
                    e.Tuple [e.Var "b1"; e.And (e.Bool true, e.Bool false); e.Int 42]],
                Type.Set (Type.Tuple [Type.Bool; Type.Bool; Type.Int]), 
                C.Set [
                    C.Tuple [C.BoolLiteral false; C.Var "b1"; C.IntLiteral 4]
                    C.Tuple [C.Var "b1"; C.And (C.BoolLiteral true, C.BoolLiteral false); C.IntLiteral 42]]
            // [t1, (true, 5)]
            e.Set [e.Var "t1"; e.Tuple [e.Bool true; e.Int 5]], 
                Type.Set (Type.Tuple [Type.Bool; Type.Int]),
                C.Set [C.Var "t1"; C.Tuple [C.BoolLiteral true; C.IntLiteral 5]]
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            e.Set [e.Var "foo"], "Variable not found: foo"
            e.Set [e.Var "i1"; e.Var "b1"; e.Var "m1"], "Invalid set element. Expected int, bool or tuple expression, got: Method ([], Type Int)"
            e.Set [e.Int 4; e.Bool true], "All elements in a set must be of the same type."
            e.Set [e.Tuple [e.Int 4; e.Bool true]; e.Tuple [e.Int 1; e.Int 2]], "All elements in a set must be of the same type."
            e.Set [e.Var "t1"; e.Tuple [e.Bool true; e.Int 5; e.Int 2]], "All elements in a set must be of the same type."
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestSetOperations () =
        let ctx = this.TypeContext

        [
            // (Element [])
            e.Element (e.Set []), Type.Tuple [], C.Element (C.Set [])
            // (Element [3])
            e.Element (e.Set [e.Int 3]), Type.Int, C.Element (C.Set [C.IntLiteral 3])
            // (Element [3,5])
            e.Set [e.Int 3; e.Int 5], 
                Type.Set (Type.Int),
                C.Set [C.IntLiteral 3; C.IntLiteral 5]
            // (Element [false, true, false, false])
            e.Set [e.Bool false; e.Bool true; e.Bool false; e.Bool false], 
                Type.Set (Type.Bool),
                C.Set [C.BoolLiteral false; C.BoolLiteral true; C.BoolLiteral false; C.BoolLiteral false]
            // (Element [(3,5)])
            e.Element (e.Set [e.Tuple [e.Int 3; e.Int 5]]), 
                Type.Tuple [Type.Int; Type.Int],
                C.Element (C.Set [C.Tuple [C.IntLiteral 3; C.IntLiteral 5]])

            // (Append 3 [])
            e.Append (e.Int 3, e.Set []), Type.Set (Type.Int), C.Append (C.IntLiteral 3, C.Set [])
            // (Add 3 [10])
            e.Append (e.Int 3, e.Set [e.Int 10]), Type.Set (Type.Int), C.Append (C.IntLiteral 3, C.Set [C.IntLiteral 10])

            // (Remove 3 [10; 3])
            e.Remove (e.Int 3, e.Set [e.Int 10; e.Int 3]), 
                Type.Set (Type.Int), 
                C.Remove (C.IntLiteral 3, C.Set [C.IntLiteral 10; C.IntLiteral 3])

            // (Remove 3 [3])
            e.Remove (e.Int 3, e.Set [e.Int 3]), 
                Type.Set (Type.Int), 
                C.Remove (C.IntLiteral 3, C.Set [C.IntLiteral 3])

            // (Count [])
            e.Count (e.Set []), 
                Type.Int, 
                C.Count (C.Set [])
            // (Count 3 [3])
            e.Count (e.Set [e.Int 3]), 
                Type.Int, 
                C.Count (C.Set [C.IntLiteral 3])

        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // Element 4
            e.Element (e.Int 4), "Element expects a Set, got: Int"
            // Element 4
            e.Count (e.Int 4), "Count expects a Set, got: Int"
            // Append (false, [3])
            e.Append (e.Bool false, e.Set [e.Int 10]), "Item to append must be of the same type as set."
            // Remove (false, [3])
            e.Remove (e.Bool false, e.Set [e.Int 10]), "Item to remove must be of the same type as set."
        ]
        |> List.iter (this.TestInvalidExpression ctx)

    [<TestMethod>]
    member this.TestAndOrNot () =
        let ctx = this.TypeContext

        [
            // (not true)
            e.Not (e.Bool true), Type.Bool, C.Not (C.BoolLiteral true)
            // (true or false) and (b1)
            e.And (e.Or (e.Bool true, e.Bool false), e.Var "b1"),
                Type.Bool,
                C.And (C.Or (C.BoolLiteral true, C.BoolLiteral false), C.Var "b1")
            // (not (b1 or false))
            e.Not (e.Or (e.Var "b1", e.Bool false)), 
                Type.Bool, 
                C.Not (C.Or (C.Var "b1", C.BoolLiteral false))
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // (not | true >)
            e.Not (e.Ket (e.Bool true)), 
                QType.Ket [Type.Bool], 
                Q.Not  (Q.Literal (Set [C.BoolLiteral true]))

            // (qb1 or k2.1) and not | true >
            e.And (e.Or (e.Var "qb1", e.Project (e.Var "k2", e.Int 1)), e.Not (e.Ket (e.Bool true))),
                QType.Ket [Type.Bool],
                Q.And (Q.Or (Q.Var "qb1", Q.Project (Q.Var "k2", 1)), (Q.Not (Q.Literal (Set [C.BoolLiteral true]))))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            e.And (e.Var "foo", e.Bool true), "Variable not found: foo"
            e.And (e.Bool true, e.Int 23), "Boolean expressions require boolean arguments, got Bool == Int"
            e.Or (e.Bool true, e.Int 23), "Boolean expressions require boolean arguments, got Bool == Int"
            e.Not (e.Int 23), "Not must be applied to a boolean expression, got: Int"
        ]
        |> List.iter (this.TestInvalidExpression ctx)



    [<TestMethod>]
    member this.TestRange() =
        let ctx = this.TypeContext

        [
            // 0..0
            e.Range (e.Int 0, e.Int 0), 
                Type.Set Type.Int,
                C.Range (C.IntLiteral 0, C.IntLiteral 0)
            // 0..3 -> [0, 1, 2]
            e.Range (e.Var "i1", e.Int 0), 
                Type.Set Type.Int,
                C.Range (C.Var "i1", C.IntLiteral 0)
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // t1 .. 10: Invalid start type
            e.Range (e.Var "t1", e.Int 0), "Start must be an int expression, got: Classic (Var \"t1\", Tuple [Bool; Int])"
            // 10 .. t1: Invalid start type
            e.Range (e.Int 10 , e.Var "t1"), "Stop must be an int expression, got: Classic (Var \"t1\", Tuple [Bool; Int])"
        ]
        |> List.iter (this.TestInvalidExpression ctx)



    [<TestMethod>]
    member this.TestMethod() =
        let ctx = this.TypeContext

        [
            // let m () = true
            e.Method ([], e.Bool true),
                Type.Method ([], Type Type.Bool),
                C.Method ([], Classic (C.BoolLiteral true, Type.Bool))
            // let m (a: Int; b: Tuple<Int, Bool>) = b
            e.Method ([("a", Type Type.Int); ("b", Type (Type.Tuple [Type.Int; Type.Bool]))], e.Var "b"),
                Type.Method (
                    [Type Type.Int; Type (Type.Tuple [Type.Int; Type.Bool])], 
                    Type (Type.Tuple [Type.Int; Type.Bool])),
                C.Method (["a"; "b"], Classic (C.Var "b",  (Type.Tuple [Type.Int; Type.Bool])))
            // let m (i:Int) = 
            //      lambda (y: Bool) = 42
            e.Method ([("i", Type Type.Int)], e.Method (["y", Type Type.Bool], e.Int 42)),
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
            e.Ket (e.Set []),
                QType.Ket [],
                Q.Literal (C.Set [])
            // |1>
            e.Ket (e.Int 1),
                QType.Ket [Type.Int],
                Q.Literal (C.Set [C.IntLiteral 1])
            // |true, false>
            e.Ket (e.Set [e.Bool true; e.Bool false]),
                QType.Ket [Type.Bool],
                Q.Literal (C.Set [C.BoolLiteral true; C.BoolLiteral false])
            // |(1,2), (3,4)>
            e.Ket (e.Set [e.Tuple [e.Int 1; e.Int 2]; e.Tuple [e.Int 3; e.Int 4]]),
                QType.Ket [Type.Int; Type.Int],
                Q.Literal (C.Set [C.Tuple [C.IntLiteral 1; C.IntLiteral 2]; C.Tuple [C.IntLiteral 3; C.IntLiteral 4]])
            // |(1,false), (3,true)>
            e.Ket (e.Set [e.Tuple [e.Int 1; e.Bool false]; e.Tuple [e.Int 3; e.Bool true]]),
                QType.Ket [Type.Int; Type.Bool],
                Q.Literal (C.Set [C.Tuple [C.IntLiteral 1; C.BoolLiteral false]; C.Tuple [C.IntLiteral 3; C.BoolLiteral true]])
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            e.Ket (e.Set [e.Bool true; e.Int 12]), "All elements in a set must be of the same type."
            e.Ket (e.Set [e.Tuple [e.Int 1; e.Bool false]; e.Tuple [e.Int 3; e.Int 4]]), "All elements in a set must be of the same type."
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestKetAll() =
        let ctx = this.TypeContext

        [
            // |@,3>
            e.KetAll (e.Int 3),
                QInt,
                Q.KetAll (C.IntLiteral 3)
            // |@,i1>
            e.KetAll (e.Var "i1"),
                QInt,
                Q.KetAll (C.Var "i1")
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            e.KetAll (e.Bool false), "Ket size must be an int expression, got: Classic (BoolLiteral false, Bool)"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestClassicAdd() =
        let ctx = this.TypeContext

        [
            // 1 + 1
            e.Add (e.Int 1, e.Int 1),
                Type.Int,
                C.Add (C.IntLiteral 1, C.IntLiteral 1)
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // No overloading:
            e.Add (e.Bool true, e.Bool false), "Add can only be applied to int expressions"
            e.Add (e.Int 1, e.Bool false), "Add can only be applied to int expressions"
            e.Add (e.Tuple [e.Int 1], e.Tuple [e.Int 1]), "Add can only be applied to int expressions"
        ]
        |> List.iter (this.TestInvalidExpression ctx)

    [<TestMethod>]
    member this.TestQuantumAdd() =
        let ctx = this.TypeContext

        [
            // |0, 1> + |1, 2, 3>
            e.Add (e.Ket (e.Set [e.Int 0;e.Int 1]), e.Ket (e.Set [e.Int 1; e.Int 2; e.Int 3])),
                QType.Ket [Type.Int],
                Q.Add (
                    Q.Literal (C.Set [C.IntLiteral 0; C.IntLiteral 1]), 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]))
            // 1 + |1, 2, 3>
            e.Add (e.Int 1, e.Ket (e.Set [e.Int 1;e.Int 2;e.Int 3])),
                QType.Ket [Type.Int],
                Q.Add (
                    Q.Literal (C.Set [C.IntLiteral 1]), 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]))
            // |1, 2, 3> + 1
            e.Add (e.Ket (e.Set [e.Int 1;e.Int 2;e.Int 3]), e.Int 1),
                QType.Ket [Type.Int],
                Q.Add (
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]),
                    Q.Literal (C.Set [C.IntLiteral 1]))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            e.Add (e.Ket (e.Set [e.Bool true; e.Int 1]), e.Ket (e.Set [e.Bool false; e.Int 2; e.Int 3])), "All elements in a set must be of the same type."
            e.Add (e.Ket (e.Set [e.Bool true]), e.Ket (e.Set [e.Bool false])), "Quantum addition can only be applied to int Kets, got Ket [Bool] + Ket [Bool]"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestClassicEquals() =
        let ctx = this.TypeContext

        [
            // 1 + 1
            e.Equals (e.Int 1, e.Int 1),
                Type.Bool,
                C.Equals (C.IntLiteral 1, C.IntLiteral 1)
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // No overloading:
            e.Equals (e.Bool true, e.Bool false), "== can only be applied to int expressions"
            e.Equals (e.Int 1, e.Bool false), "== can only be applied to int expressions"
            e.Equals (e.Tuple [e.Int 1], e.Tuple [e.Int 1]), "== can only be applied to int expressions"
        ]
        |> List.iter (this.TestInvalidExpression ctx)

    [<TestMethod>]
    member this.TestQuantumEquals() =
        let ctx = this.TypeContext

        [
            // |0, 1> = |1, 2, 3>
            e.Equals (e.Ket (e.Set [e.Int 0;e.Int 1]), e.Ket (e.Set [e.Int 1; e.Int 2; e.Int 3])),
                QType.Ket [Type.Bool],
                Q.Equals (
                    Q.Literal (C.Set [C.IntLiteral 0; C.IntLiteral 1]), 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]))
            // 1 = |1, 2, 3>
            e.Equals (e.Int 1, e.Ket (e.Set [e.Int 1;e.Int 2;e.Int 3])),
                QType.Ket [Type.Bool],
                Q.Equals (
                    Q.Literal (C.Set [C.IntLiteral 1]), 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]))
            // |1, 2, 3> = 1
            e.Equals (e.Ket (e.Set [e.Int 1;e.Int 2;e.Int 3]), e.Int 1),
                QType.Ket [Type.Bool],
                Q.Equals (
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]),
                    Q.Literal (C.Set [C.IntLiteral 1]))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            e.Equals (e.Ket (e.Set [e.Bool true; e.Int 1]), e.Ket (e.Set [e.Bool false; e.Int 2; e.Int 3])), "All elements in a set must be of the same type."
            e.Equals (e.Ket (e.Set [e.Bool true]), e.Ket (e.Set [e.Bool false])), "Quantum == can only be applied to int Kets"
        ]
        |> List.iter (this.TestInvalidExpression ctx)

    [<TestMethod>]
    member this.TestClassicMultiply() =
        let ctx = this.TypeContext

        [
            // 1 * 1
            e.Multiply (e.Int 1, e.Int 1),
                Type.Int,
                C.Multiply (C.IntLiteral 1, C.IntLiteral 1)
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // No overloading:
            e.Multiply (e.Bool true, e.Bool false), "Multiply can only be applied to int expressions"
            e.Multiply (e.Int 1, e.Bool false), "Multiply can only be applied to int expressions"
            e.Multiply (e.Tuple [e.Int 1], e.Tuple [e.Int 1]), "Multiply can only be applied to int expressions"
        ]
        |> List.iter (this.TestInvalidExpression ctx)

    [<TestMethod>]
    member this.TestQuantumMultiply() =
        let ctx = this.TypeContext

        [
            // |0, 1> * |1, 2, 3>
            e.Multiply (e.Ket (e.Set [e.Int 0;e.Int 1]), e.Ket (e.Set [e.Int 1; e.Int 2; e.Int 3])),
                QType.Ket [Type.Int],
                Q.Multiply (
                    Q.Literal (C.Set [C.IntLiteral 0; C.IntLiteral 1]), 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]))
            // 1 * |1, 2, 3>
            e.Multiply (e.Int 1, e.Ket (e.Set [e.Int 1;e.Int 2;e.Int 3])),
                QType.Ket [Type.Int],
                Q.Multiply (
                    Q.Literal (C.Set [C.IntLiteral 1]), 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]))
            // |1, 2, 3> * 1
            e.Multiply (e.Ket (e.Set [e.Int 1;e.Int 2;e.Int 3]), e.Int 1),
                QType.Ket [Type.Int],
                Q.Multiply (
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]),
                    Q.Literal (C.Set [C.IntLiteral 1]))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            e.Multiply (e.Ket (e.Set [e.Bool true; e.Int 1]), e.Ket (e.Set [e.Bool false; e.Int 2; e.Int 3])), "All elements in a set must be of the same type."
            e.Multiply (e.Ket (e.Set [e.Bool true]), e.Ket (e.Set [e.Bool false])), "Quantum multiplication can only be applied to int Kets"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestLessThan() =
        let ctx = this.TypeContext

        [
            // 1 < 1
            e.LessThan (e.Int 1, e.Int 1),
                Type.Bool,
                C.LessThan (C.IntLiteral 1, C.IntLiteral 1)
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            e.LessThan (e.Bool true, e.Bool false), "Both expressions for < must be int. Got Bool < Bool"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestCallMethod() =
        let ctx = 
            this.TypeContext
            |> add_to_typecontext ([
                "m2", AnyType.Type (
                    Type.Method (
                        [AnyType.QType (QType.Ket [Type.Int]); AnyType.Type (Type.Tuple [Type.Int; Type.Bool])], 
                        AnyType.Type (Type.Tuple [Type.Int; Type.Int])))
                "q2", AnyType.Type (
                    Type.Method (
                        [AnyType.QType (QType.Ket [Type.Int]); AnyType.Type (Type.Tuple [Type.Int; Type.Bool])], 
                        AnyType.QType (QType.Ket [Type.Bool])))
            ])

        [
            // m1()
            e.CallMethod (e.Var "m1", []),
                Type.Int,
                C.CallMethod (C.Var ("m1"), [])
            // m2(Ket<Int>, Tuple<Int, Bool>) : Tuple<Int, Int>
            e.CallMethod (e.Var "m2", [e.Ket (e.Int 1); e.Tuple [e.Int 2; e.Bool false]]),
                Type.Tuple [Type.Int; Type.Int],
                C.CallMethod (C.Var ("m2"),  [
                    Quantum (Q.Literal (C.Set [C.IntLiteral 1]), QType.Ket [Type.Int]); 
                    Classic (C.Tuple [C.IntLiteral 2; C.BoolLiteral false], Type.Tuple[Type.Int; Type.Bool])])
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // q1()
            e.CallMethod (e.Var "q1", []),
                QType.Ket [Type.Int],
                Q.CallMethod (C.Var ("q1"), [])
            // m2(Ket<Int>, (Int, Bool) : Ket<Bool>
            e.CallMethod (e.Var "q2", [e.Ket (e.Int 1); e.Tuple [e.Int 2; e.Bool false]]),
                QType.Ket [Type.Bool],
                Q.CallMethod (C.Var ("q2"),  [
                    Quantum (Q.Literal (C.Set [C.IntLiteral 1]), QType.Ket[Type.Int]); 
                    Classic (C.Tuple [C.IntLiteral 2; C.BoolLiteral false], Type.Tuple[Type.Int; Type.Bool])])
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            e.CallMethod (e.Var "m1", [e.Int 1]), "Arguments type missmatch. Expected [] got [Type Int]"
            e.CallMethod (e.Var "q1", [e.Int 1]), "Arguments type missmatch. Expected [] got [Type Int]"
            e.CallMethod (e.Var "m2", [e.Int 1; e.Tuple [e.Int 2; e.Bool false]]), "Arguments type missmatch. Expected [QType (Ket [Int]); Type (Tuple [Int; Bool])] got [Type Int; Type (Tuple [Int; Bool])]"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestJoin() =
        let ctx = this.TypeContext

        [
            // ((), ())
            e.Join (e.Tuple [], e.Tuple []),
                Type.Tuple [],
                C.Join (C.Tuple [], C.Tuple [])
            // (t1, ())
            e.Join (e.Var "t1", e.Tuple []),
                Type.Tuple [Type.Bool; Type.Int],
                C.Join (C.Var "t1", C.Tuple [])
            // ((1,1), (0,0,false))
            e.Join (e.Tuple [e.Int 1; e.Int 1], e.Tuple [e.Int 0; e.Int 0; e.Bool true]),
                Type.Tuple [Type.Int; Type.Int; Type.Int; Type.Int; Type.Bool],
                C.Join (C.Tuple [C.IntLiteral 1; C.IntLiteral 1], C.Tuple[C.IntLiteral 0; C.IntLiteral 0; C.BoolLiteral true])
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // (|>|), |>)
            e.Join (e.Ket (e.Set []), e.Ket (e.Set [])),
                QType.Ket [],
                Q.Join (Q.Literal (C.Set []), Q.Literal (C.Set []))
            // (t1, |>)
            e.Join (e.Var "k1", e.Ket (e.Set [])),
                QType.Ket [Type.Int],
                Q.Join (Q.Var "k1", Q.Literal (C.Set []))
            // (t1, |1,2,3>)
            e.Join (e.Var "k1", e.Ket (e.Set [e.Int 1; e.Int 2; e.Int 3])),
                QType.Ket [Type.Int; Type.Int],
                Q.Join (
                    Q.Var "k1", 
                    Q.Literal (C.Set [C.IntLiteral 1; C.IntLiteral 2; C.IntLiteral 3]))
            // (|(1,1)>, |(0,0,false)>)
            e.Join (e.Ket (e.Set [e.Tuple [e.Int 1; e.Int 1]]), e.Ket (e.Set [e.Tuple [e.Int 0; e.Int 0; e.Bool true]])),
                QType.Ket [Type.Int; Type.Int; Type.Int; Type.Int; Type.Bool],
                Q.Join (
                    Q.Literal (C.Set [C.Tuple [C.IntLiteral 1; C.IntLiteral 1]]),
                    Q.Literal (C.Set [C.Tuple [C.IntLiteral 0; C.IntLiteral 0; C.BoolLiteral true]]))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            e.Join (e.Bool true, e.Int 1), "Join is only supported on tuples and kets, got: Classic (BoolLiteral true, Bool) , Classic (IntLiteral 1, Int)"
        ]
        |> List.iter (this.TestInvalidExpression ctx)



    [<TestMethod>]
    member this.TestProject() =
        let ctx = this.TypeContext

        [
            // (1).0
            e.Project (e.Tuple [Int 1], Int 0),
                Type.Int,
                C.Project (C.Tuple [C.IntLiteral 1], 0)
            // (1, false, 3, true).3
            e.Project (e.Tuple [Int 1; e.Bool false; Int 3; e.Bool true], Int 3),
                Type.Bool,
                C.Project (C.Tuple [C.IntLiteral 1; C.BoolLiteral false; C.IntLiteral 3; C.BoolLiteral true], 3)
            // (1, false, 3, true).2
            e.Project (e.Tuple [Int 1; e.Bool false; Int 3; e.Bool true], Int 2),
                Type.Int,
                C.Project (C.Tuple [C.IntLiteral 1; C.BoolLiteral false; C.IntLiteral 3; C.BoolLiteral true], 2)
            // (1).i1
            e.Project (e.Tuple [Int 1], e.Var "i1"),
                Type.Int,
                C.Index (C.Tuple [C.IntLiteral 1], C.Var "i1")
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // |1>.0
            e.Project (e.Ket (e.Set [Int 1]), Int 0),
                QType.Ket [Type.Int],
                Q.Project (Q.Literal (C.Set [C.IntLiteral 1]), 0)
            // k1.2  --> Note, index in projection is modular, so 2 == 0
            e.Project (e.Var "k1", Int 2),
                QType.Ket [Type.Int],
                Q.Project (Q.Var "k1", 0)
            // k2.1
            e.Project (e.Var "k2", Int 1),
                QType.Ket [Type.Bool],
                Q.Project (Q.Var "k2", 1)
            // |(1, false, 3, true)>.2
            e.Project (e.Ket (e.Set [e.Tuple [Int 1; e.Bool false; Int 3; e.Bool true]]), Int 2),
                QType.Ket [Type.Int],
                Q.Project (Q.Literal (C.Set [Tuple [C.IntLiteral 1; C.BoolLiteral false; C.IntLiteral 3; C.BoolLiteral true]]), 2)
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            // (1, false, 3).i1
            e.Project (e.Tuple [Int 1; e.Bool false; Int 3], e.Var "i1"), "Indexing of tuples is only available on tuples of a single type"
            // |(1, false, 3)>.i1
            e.Project (e.Ket (e.Set [e.Tuple [Int 1; e.Bool false; Int 3]]), e.Var "i1"), "Indexing of kets is only available on kets of a single type"
            // [(1, false, 3)].[0]
            e.Project (e.Set [e.Tuple [Int 1; e.Bool false; Int 3]], e.Int 2), "Project is only supported on tuples and kets"
            // [(1, false, 3)].[i1]
            e.Project (e.Set [e.Tuple [Int 1; e.Bool false; Int 3]], e.Var "i1"), "Project is only supported on tuples and kets"
            // [1, 2, 3].[false]
            e.Project (e.Set [Int 1; e.Int 2; Int 3], e.Bool false), "Invalid projection index. Expected int expression, got: Classic (BoolLiteral false, Bool)"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestBlock() =
        let ctx = this.TypeContext

        [
            // { t1 }
            e.Block ([], (e.Var "t1")),
                Type.Tuple [Type.Bool; Type.Int],
                C.Block ([], C.Var "t1")
            // { let a = 15; print "some msg" a k1; a + i1 }
            e.Block ([
                s.Let ("a", e.Int 15)
                s.Print ("some msg", [e.Var "a"; e.Var "k1"])],
                e.Add (e.Var "a", e.Var "i1")),
                Type.Int,
                C.Block ([
                    Let ("a", Classic (C.IntLiteral 15, Type.Int))
                    Print ("some msg", [Classic (C.Var "a", Type.Int); (Quantum (Q.Var "k1", QType.Ket [Type.Int]))])],
                    C.Add (C.Var "a", C.Var "i1"))
            // { let a = 15; let b = a + 2; b }
            e.Block ([
                s.Let ("a", e.Int 15)
                s.Let ("b", e.Add (e.Var "a", e.Int 2))],
                e.Var "b"),
                Type.Int,
                C.Block ([
                    Let ("a", Classic (C.IntLiteral 15, Type.Int))
                    Let ("b", Classic (C.Add(C.Var "a", C.IntLiteral 2), Type.Int))],
                    C.Var "b")            
            // { let a = 15; let b = a + 2; a = true; (a, b) }
            e.Block ([
                s.Let ("a", e.Int 15)
                s.Let ("b", e.Add (e.Var "a", e.Int 2))
                s.Let ("a", e.Bool true)],
                e.Tuple [e.Var "a"; e.Var "b"]),
                Type.Tuple [Type.Bool; Type.Int],
                C.Block ([
                    Let ("a", Classic (C.IntLiteral 15, Type.Int))
                    Let ("b", Classic (C.Add(C.Var "a", C.IntLiteral 2), Type.Int))
                    Let ("a", Classic (C.BoolLiteral true, Type.Bool))],
                    C.Tuple [C.Var "a"; C.Var "b"])
            // { let a = 15; let b = { let a = true; a }; (a, b) }
            e.Block ([
                s.Let ("a", e.Int 15)
                s.Let ("b", e.Block ([
                    s.Let("a", e.Bool true)],
                    e.Var "a"))
                ],
                e.Tuple [e.Var "a"; e.Var "b"]),
                Type.Tuple [Type.Int; Type.Bool],
                C.Block ([
                    Let ("a", Classic (C.IntLiteral 15, Type.Int))
                    Let ("b", Classic (C.Block ([
                        Let ("a", Classic (C.BoolLiteral true, Type.Bool))],  
                        C.Var "a"), Type.Bool))],
                    C.Tuple [C.Var "a"; C.Var "b"])

        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // { t1 }
            e.Block ([], (e.Var "k1")),
                QType.Ket [Type.Int],
                Q.Block ([], Q.Var "k1")
            // { let a = 15; print "some msg" a k1; a + k1 }
            e.Block ([
                s.Let ("a", e.Int 15)
                s.Print ("some msg", [e.Var "a"; e.Var "k1"])],
                (e.Add (e.Var "a", e.Var "k1"))),
                QType.Ket [Type.Int],
                Q.Block ([
                    Let ("a", Classic (C.IntLiteral 15, Type.Int))
                    Print ("some msg", [Classic (C.Var "a", Type.Int); (Quantum (Q.Var "k1", QType.Ket [Type.Int]))])],
                    Q.Add (Q.Literal (C.Set [C.Var "a"]), Q.Var "k1"))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            // { let a = 5 == false; a}
            e.Block ([s.Let ("a", e.Equals (e.Int 5, e.Bool false))], e.Var "a"), "== can only be applied to int expressions"
            // { let a = { let b = 2; b } b }
            e.Block ([
                s.Let ("a", e.Block ([s.Let ("b", e.Int 2)], e.Var "b"))],
                e.Var "b"), 
                "Variable not found: b"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestIf() =
        let ctx = this.TypeContext

        [
            // { if true then 1 else 0 }
            e.If (e.Bool true, e.Int 1, e.Int 0),
                Type.Int,
                C.If (C.BoolLiteral true, C.IntLiteral 1, C.IntLiteral 0)
            // { if b1 or t1.0 then (0, 1) else (0, 0) }
            e.If (e.Or (e.Var "b1", e.Project (e.Var "t1", e.Int 0)), e.Tuple [e.Int 0; e.Int 1], e.Tuple [e.Int 0; e.Int 0]),
                Type.Tuple [Type.Int; Type.Int],
                C.If (C.Or (C.Var "b1", C.Project (C.Var "t1", 0)), C.Tuple [C.IntLiteral 0; C.IntLiteral 1], C.Tuple [C.IntLiteral 0; C.IntLiteral 0])
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // { if true then k1 else |0> }
            e.If (e.Bool true, e.Var "k1", e.Ket (e.Set [e.Int 0])),
                QType.Ket [Type.Int],
                Q.IfClassic (C.BoolLiteral true, Q.Var "k1", Q.Literal (Set [C.IntLiteral 0]))
            // { if true then k1 else 0 }
            e.If (e.Bool true, e.Var "k1", e.Int 0),
                QType.Ket [Type.Int],
                Q.IfClassic (C.BoolLiteral true, Q.Var "k1", Q.Literal (Set [C.IntLiteral 0]))
            // { if k2.1 then (0, 1) else (0, 0) }
            e.If (e.Project (e.Var "k2", e.Int 1), e.Tuple [e.Int 0; e.Int 1], e.Tuple [e.Int 0; e.Int 0]),
                QType.Ket [Type.Int; Type.Int],
                Q.IfQuantum (
                    Q.Project (Q.Var "k2", 1), 
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
            e.If (e.Bool true, e.Int 1, e.Bool false), "Both branches of if statement must be of the same type, got Int and Bool"
            // { if true then k1 else k2 }
            e.If (e.Bool true, e.Var "k1", e.Var "k2"), "Both branches of if statement must be of the same type, got Ket [Int] and Ket [Int; Bool]"
            // { if 42 then 1 else 2 }
            e.If (e.Int 42, e.Int 1, e.Int 2), "If condition must be a boolean, got Int"
            // { if 42 then |1> else |0> }
            e.If (e.Int 42, e.Ket (e.Set [e.Int 1]), e.Ket (e.Int 0)), "If condition must be a boolean, got Int"
        ]
        |> List.iter (this.TestInvalidExpression ctx)



    [<TestMethod>]
    member this.TestSummarize() =
        let ctx = this.TypeContext

        [
            // summarize e in s1 with and { e.1 < 10 } 
            e.Summarize ("e", e.Var "s1", Aggregation.And, e.LessThan (e.Project (e.Var "e", e.Int 1), e.Int 10)),
                Type.Bool,
                C.Summarize ("e", C.Var "s1", Aggregation.And, C.LessThan (C.Project (C.Var "e", 1), C.IntLiteral 10))
            // summarize e in s1 with or { e.1 == 10 } 
            e.Summarize ("e", e.Var "s1", Aggregation.Or, e.Equals (e.Project (e.Var "e", e.Int 1), e.Int 10)),
                Type.Bool,
                C.Summarize ("e", C.Var "s1", Aggregation.Or, C.Equals (C.Project (C.Var "e", 1), C.IntLiteral 10))
            // summarize e in [true, false] with or { true } 
            e.Summarize ("e", e.Set [e.Bool true; e.Bool false], Aggregation.Or, e.Bool true),
                Type.Bool,
                C.Summarize ("e", C.Set [C.BoolLiteral true; C.BoolLiteral false], Aggregation.Or, C.BoolLiteral true)
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            // summarize e in s1 with and { k2.0 == e.1 } 
            e.Summarize ("e", e.Var "s1", Aggregation.And, e.Equals (e.Project (e.Var "k2", e.Int 0), e.Project (e.Var "e", e.Int 1))),
                QType.Ket [Type.Bool],
                Q.Summarize ("e", C.Var "s1", Aggregation.And, Q.Equals (Q.Project (Q.Var "k2", 0), (Q.Literal (C.Set [C.Project (C.Var "e", 1)]))))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            // summarize e in s1 with and { e < 10 } 
            e.Summarize ("e", e.Var "s1", Aggregation.And, e.LessThan ((e.Var "e"), e.Int 10)), "Both expressions for < must be int. Got Tuple [Bool; Int] < Int"
            // summarize e in t1 with and { e.0 == true } 
            e.Summarize ("e", e.Var "t1", Aggregation.And, e.Equals (e.Project (e.Var "e", e.Int 0), e.Bool true)), "Summarize expects a classic set of values, got: Tuple [Bool; Int]"
            // summarize e in k1 with and { e.0 == |1> } 
            e.Summarize ("e", e.Var "k1", Aggregation.And, e.Equals (e.Project (e.Var "e", e.Int 0), e.Ket (e.Int 1))), "Summarize expects a classic set of values, got: Ket [Int]"
            // summarize e in s1 with and { k2.1 == e.1 } 
            e.Summarize ("e", e.Var "s1", Aggregation.And, e.Equals (e.Project (e.Var "k2", e.Int 1), e.Project (e.Var "e", e.Int 1))), "Quantum == can only be applied to int Kets"
            // summarize e in s1 with sum { true } 
            e.Summarize ("e", e.Var "s1", Aggregation.Sum, e.Bool true), "Summarize body must be an Int expression when aggregation is 'sum', got Bool"
            // summarize e in s1 with and { 1 } 
            e.Summarize ("e", e.Var "s1", Aggregation.And, e.Int 1), "Summarize body must be a boolean expression when aggregation is 'and' | 'or', got Int"
            // summarize e in s1 with or { 1 } 
            e.Summarize ("e", e.Var "s1", Aggregation.Or, e.Int 1), "Summarize body must be a boolean expression when aggregation is 'and' | 'or', got Int"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestSolve() =
        let ctx = this.TypeContext

        [
            // k2 | k2.[0] == 1
            e.Solve(e.Var "k2", e.Equals (e.Project (e.Var "k2", e.Int 0), e.Int 1)),
                QType.Ket [Type.Int; Type.Bool],
                Q.Solve(Q.Var "k2", Q.Equals (Q.Project (Q.Var "k2", 0), Q.Literal (C.Set [C.IntLiteral 1])))
        ]
        |> List.iter (this.TestQuantumExpression ctx)

        [
            e.Solve (e.Tuple [e.Int 1;e.Int 2], e.Equals (e.Project (e.Var "k2", e.Int 1), e.Bool true)), "Solve argument must be a quantum ket, got: Tuple [Int; Int]"
            e.Solve (e.Var "k2", e.Bool true), "Solve condition must be a quantum boolean expression, got: Bool"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestSample() =
        let ctx = this.TypeContext

        [
            // | |> |
            e.Sample (e.Prepare (e.Ket (e.Set []))),
                Type.Tuple [],
                C.Sample( U.Prepare( Q.Literal (C.Set [])))
            // | k1 |
            e.Sample (e.Prepare (e.Var "k1")),
                Type.Tuple [Type.Int],
                C.Sample(U.Prepare (Q.Var "k1"))
            // | k2 |
            e.Sample (e.Prepare (e.Var "k2")),
                Type.Tuple [Type.Int; Type.Bool],
                C.Sample(U.Prepare (Q.Var "k2"))
        ]
        |> List.iter (this.TestClassicExpression ctx)

        [
            e.Sample (e.Tuple [e.Int 1;e.Int 2]), "Sample argument must be a quantum ket, got: Tuple [Int; Int]"
        ]
        |> List.iter (this.TestInvalidExpression ctx)


    [<TestMethod>]
    member this.TestCaptureVariables () =
        let ctx = this.TypeContext

        [
            // let alpha = true
            // let beta = false
            // let foo(beta:Int) =
            //    let alpha = if alpha then 3 * beta else 0
            //    let x =
            //      let beta = 10 
            //      let alpha = alpha * beta
            //      alpha
            //    (alpha, beta, x)
            // ((alpha, beta), foo(25))
            e.Block ([
                s.Let ("alpha", e.Bool true)
                s.Let ("beta", e.Bool false)
                s.Let ("foo",
                    e.Method(
                        ["beta", AnyType.Type Type.Int],
                        e.Block ([
                            s.Let ("alpha", e.If(
                                e.Var "alpha",
                                e.Multiply(e.Int 3, e.Var "beta"),
                                e.Int 0))
                            s.Let ("x", e.Block ([
                                s.Let ("beta", e.Int 11)
                                s.Let ("alpha", e.Multiply(e.Var "alpha", e.Var "beta"))
                            ], e.Var "alpha"))
                        ], e.Tuple [e.Var "alpha"; e.Var "beta"; e.Var "x"])))
            ], e.Join (
                e.Tuple [
                    e.Var "alpha"
                    e.Var "beta"],
                e.CallMethod (e.Var "foo", [e.Int 25])
            )),
            Type.Tuple [Type.Bool; Type.Bool; Type.Int; Type.Int; Type.Int],
            C.Block ([
                Statement.Let ("alpha", Classic (C.BoolLiteral true, Type.Bool))
                Statement.Let ("beta", Classic (C.BoolLiteral false, Type.Bool))
                Statement.Let ("foo", Classic (C.Method(
                    arguments = ["beta"], 
                    body = Classic (
                        C.Block ([
                            Statement.Let ("alpha", Classic (
                                C.If(
                                    C.Var "alpha",
                                    C.Multiply (C.IntLiteral 3, C.Var "beta"),
                                    C.IntLiteral 0), Type.Int))
                            Statement.Let ("x", Classic (C.Block ([
                                Statement.Let ("beta", Classic (C.IntLiteral 11, Type.Int))
                                Statement.Let ("alpha", Classic (C.Multiply (C.Var "alpha", C.Var "beta"), Type.Int))
                            ], C.Var "alpha"), Type.Int))
                        ], C.Tuple [
                            C.Var "alpha"; 
                            C.Var "beta"; 
                            C.Var "x" ]), 
                        Type.Tuple [Type.Int; Type.Int; Type.Int])), 
                    Type.Method ([AnyType.Type Type.Int], AnyType.Type (Type.Tuple [Type.Int; Type.Int; Type.Int]))))
            ], C.Join( 
                C.Tuple [C.Var "alpha"; C.Var "beta" ],
                C.CallMethod (C.Var "foo", [Classic (C.IntLiteral 25, Type.Int)])))
        ]
        |> List.iter (this.TestClassicExpression ctx)
