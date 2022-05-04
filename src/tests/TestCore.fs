namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.runtime.Core
open aleph.parser.core

[<TestClass>]
type TestCore () =

    let onlyCore = { 
        new RuntimeExtension<unit, int> with
            override this.Eval core (e, ctx) = core (e, ctx)
    }

    let eval = (evalCore onlyCore)

    member this.TestExpression (e, expected, ?ctx)=
        let ctx = defaultArg ctx (Context (Map.empty, eval))

        match eval (e, ctx) with
        | Ok (actual, _) -> 
            Assert.AreEqual(expected.ToString(), actual.ToString())
        | Error msg -> 
            Assert.AreEqual(expected, msg)
            Assert.Fail()
        
    member this.TestInvalidExpression (e, expected, ?ctx)=
        let ctx = defaultArg ctx (Context (Map.empty, eval))
        
        match eval (e, ctx)  with
        | Ok (v, _) ->
            Assert.AreEqual($"ERROR for {e}", $"Expression returned: {v}")
        | Error actual -> 
            Assert.AreEqual(expected, actual)

    member this.Context =
        let i1 = Value.Tuple [I 3]
        let b1 = Value.Tuple [B false]
        let t1 = Value.Tuple [B false; I 5]
        let t2 = Value.Tuple [B true; I 12]
        let s1 = Value.Set(SET [
            [B false; I 5]
            [B true; I 12]
        ])
        // classic = plusone (a) -> a + 1
        let plusone = Value.Method (["a"], Add([Id "a"; Int 1]))

        Context (Map[ 
            ("i1", i1)
            ("b1", b1)
            ("t1", t1)
            ("t2", t2)
            ("s1", s1)
            ("plusone", plusone)
        ], eval)


    [<TestMethod>]
    member this.LiteralExpressions() =
        this.TestExpression(Int(5), Value.Int 5)
        this.TestExpression(Bool(true), Value.Bool true)
        this.TestExpression(Bool(false), Value.Bool false)


    [<TestMethod>]
    member this.IdExpressions() =
        let ctx = this.Context

        for i in ctx.Map.Keys do
            this.TestExpression(Id(i), ctx.Map.[i], ctx)

        // Some negative cases too:
        this.TestInvalidExpression(Id("i1"), "Unknown variable: i1", (Context (Map.empty, eval)))

        Assert.IsTrue((ctx.TryFind "foo").IsNone)
        this.TestInvalidExpression(Id("foo"), "Unknown variable: foo", ctx)


    [<TestMethod>]
    member this.TuplesExpressions() =
        let ctx = this.Context

        [
            // () -> ()
            (Tuple([]), Value.Tuple([]))
            // (3) -> (3)
            (Tuple([Int(3)]), Value.Tuple([I(3)]))
            // (3,5) -> (3,5)
            (Tuple([Int(3); Int(5)]), Value.Tuple([I(3); I(5)]))
            // (f, b1, t) -> (f, f, t)
            (Tuple([Bool(false); Id("b1"); Bool(true)]), Value.Tuple([B(false); B(false); B(true)]))
            // (i1, i1) -> (3, 3)
            (Tuple([Id("i1"); Id("i1")]), Value.Tuple([I 3; I 3]))
            // ((3)) --> (3)
            (Tuple([Tuple([Int(3)])]), Value.Tuple([I(3)]))
            // ((0,1)) --> (0,1)
            (Tuple([Tuple([Int(0); Int(1)])]), Value.Tuple([I(0); I(1)]))
            // ((1,2),(3,4)) = (1,2,3,4)
            (Tuple([
                    Tuple([Int(1); Int(2)])
                    Tuple([Int(3); Int(4)])]),
                Value.Tuple([I(1); I(2); I(3); I(4)]))
            // ((0,1), 2, ((3, 4), 5)) --> (0,1,2,3,4,5)
            (Tuple([
                    Tuple([Int(0); Int(1)])
                    Int(2)
                    Tuple([
                        Tuple([Int(3); Int(4)])
                        Int(5)
                    ])]),
                Value.Tuple([I(0); I(1); I(2); I(3); I(4); I(5)]))
            // ([0,1], 2) -> [(0,2), (1,2)]
            (Tuple([
                    Set([Int(0); Int(1)])
                    Int(2)]), 
                Value.Set(SET [
                    [I(0); I(2)]
                    [I(1); I(2)]]))
            // ( [ (0,1,2), (3,4,5) ], [ (6,F), (7,T) ] ) -> [(0,1,2,6,F), (0,1,2,7,T), (3,4,5,6,F), (3,4,5,7,T)]
            (Tuple([
                    Set([
                        Tuple([Int(0); Int(1); Int(2)])
                        Tuple([Int(3); Int(4); Int(5)])
                    ])
                    Set([
                        Tuple([Int(6); Bool(false)])
                        Tuple([Int(7); Bool(true)])])
                ]), Value.Set(SET [
                        [I(0); I(1); I(2); I(6); B(false)]
                        [I(0); I(1); I(2); I(7); B(true)]
                        [I(3); I(4); I(5); I(6); B(false)]
                        [I(3); I(4); I(5); I(7); B(true)]
            ]))
            // ( s1 ) -> [ (F, 5), (T, 12) ]
            (Tuple([Id("s1")]), Value.Set(SET([
                [B(false); I(5)]
                [B(true); I(12)]
            ])))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            // TODO: this should fail: ( [1,2,3] )
            //Tuple [ Set [Int 1; Int 2; Int 3] ]
            // Tuple with invalid id
            (Tuple [ Id("foo") ], "Unknown variable: foo")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))



    [<TestMethod>]
    member this.SetExpressions() =
        let ctx = this.Context

        // ---------------------------------- //
        // Positive cases
        // ---------------------------------- //
        [
            // []
            //(Set([]), Value.Set(SET([])))
            
            // [[], []] -> []
            (Set([
                Set([])
                Set([])
            ]), Value.Set(SET([])))

            
            // [[], (3, 4) []] -> [(3, 4)]
            (Set([
                Set([])
                Tuple([Int(3); Int(4)])
                Set([])
            ]), Value.Set(SET([[I 3; I 4]])))

            // [0] -> [(0)]
            (Set([Int(0)]), Value.Set(SET([
                [I(0)]
            ])))

            // [0, (1)] -> [(0), (1)]
            (Set [
                Int(0)
                Tuple([Int(1)])
            ], Value.Set(SET [
                [I(0)]
                [I(1)]
            ]))
            
            
            // [1, 2, 1] --> [(1), (2)]
            (Set([Int(1); Int(2); Int(1)]), Value.Set(SET([
                [I(1)]
                [I(2)]
            ])))
            
            //[(1,t), (3,f)] --> [(1,t), (3,f)]
            (Set([
                Tuple[Int(1); Bool(true)]
                Tuple[Int(3); Bool(false)]
            ]), Value.Set(SET([
                [I(1); B(true)]
                [I(3); B(false)]
            ])))
            
            // given t1 = (f, 5)
            // [ t1 ] --> [ (f, 5) ]  
            (Set([Id("t1")]), Value.Set(SET([
                [B(false); I(5)]
            ])))

            // given t1 = (f, 5)
            // [ t1; t1 ] --> [ (f, 5) ]
            (Set([Id("t1");Id("t1")]), Value.Set(SET([
                [B(false); I(5)]
            ])))
            
            // [ t1, (T, 12) ] --> [ (f, 5), (T, 12) ]
            (Set([
                Id("t1")
                Tuple([Bool(true); Int(12)])
            ]), Value.Set(SET([
                [B(false); I(5)]
                [B(true); I(12)]
            ])))
            
            //  given s1 = [ (F, 5), (T,12) ]
            // [ s1 ] -> [ (F, 5), (T,12) ]
            (Set([Id("s1")]), ctx.Map["s1"])            // Tuple with set inside

            //  given s1 = [ (F, 5), (T,12) ]
            // [ s1, s1 ] -> [ (F, 5), (T,12) ]
            (Set([Id("s1");Id("s1")]), ctx.Map["s1"])            // Tuple with set inside

            // [ [ (0,0), (1,1) ], (0,1), (1,1)  ] --> [ (0,0), (0,1), (1,1) ] ]
            (Set([
                Set([
                    Tuple([Int(0); Int(0)])
                    Tuple([Int(1); Int(1)])
                ])
                Tuple([Int(0); Int(1)])
                Tuple([Int(1); Int(1)])
            ]), Value.Set(SET([
                [I(0); I(0)]
                [I(0); I(1)]
                [I(1); I(1)]
            ])))

            // [ [ (0,0,0), (1,1,1) ], [(0,1,0), (1,1,1)]  ] --> [ (0,0,0), (0,1,0), (1,1,1) ]
            (Set([
                Set([
                    Tuple([Int(0); Int(0); Int(0)])
                    Tuple([Int(1); Int(1); Int(1)])
                ])
                Set([
                    Tuple([Int(0); Int(1); Int(0)])
                    Tuple([Int(1); Int(1); Int(1)])
                ])
            ]), Value.Set(SET([
                [I(0); I(0); I(0)]
                [I(0); I(1); I(0)]
                [I(1); I(1); I(1)]
            ])))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        // ---------------------------------- //
        // Negative cases
        // ---------------------------------- //
        [
            //[1, 2, (2,3)]
            (Set [
                Int(1)
                Int(2)
                Tuple([ Int(2); Int(3) ])
            ], "All tuples must have the same length. [1] != [2; 3]")
            //[1, (2, 3)] : Uneven lenghts
            (Set [
                Int(1)
                Tuple([Int(2);Int(3)]) 
            ], "All tuples must have the same length. [1] != [2; 3]")
            //[ [(0,0), (1,2)], [(2, 3, 4)] ] : Uneven embeded lenghts
            (Set [
                Set [
                    Tuple([Int(0); Int(0)])
                    Tuple([Int(1); Int(2)])
                ]
                Set[
                    Tuple([Int(2); Int(3); Int(4)])
                ]
            ], "All tuples must have the same length. [0; 0] != [2; 3; 4]")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))


    [<TestMethod>]
    member this.RangeExpressions() =
        let ctx = this.Context

        [
            // 0..0 -> []
            (Range(Int(0), Int(0)), Value.Set(SET []))
            // 0..3 -> [0, 1, 2]
            (Range(Int(0), Int(3)), Value.Set (SET [[I 0]; [I 1]; [I 2]]))
            // i1..4 --> [3]
            (Range(Id("i1"), Int(4)), Value.Set(SET [[I 3]]))
            // TODO: this feels wrong: ( 0..i1 ) -> [0, 1, 2]
            (Tuple [Range(Int(0), Id("i1"))], Value.Set (SET [[I 0]; [I 1]; [I 2]]))
            // [ 0..3 ] -> [0, 1, 2]
            (Set [Range(Int(0), Int(3))], Value.Set (SET [[I 0]; [I 1]; [I 2]]))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            // t1 .. 10: Invalid start type
            (Range (Id("t1"), Int(0)), "Invalid value for a range start..end: (False, 5)..0")
            // 10 .. t1: Invalid start type
            (Range (Int(10), Id("t1")), "Invalid value for a range start..end: 10..(False, 5)")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))


    [<TestMethod>]
    member this.EvalArithmeticExpressions() =
        let ctx = this.Context

        [
            // 0 + 1 -> 1
            (Add [Int 0; Int 1], Value.Int 1)
            // 0 + (1) + 2 + 4-> 7
            (Add [Int 0; Tuple[Int 1]; Int 2; Int 4], Value.Int 7)
            
            // Boolean is + mod 2
            // f + t -> t
            (Add [Bool false; Bool true], Value.Bool true)
            // t + f -> t
            (Add [Bool true; Bool false], Value.Bool true)
            // f + f -> f
            (Add [Bool false; Bool false], Value.Bool false)
            // t + t -> f

            // Adding/multiplying tuples, is item-wise:
            (Add [Bool true; Bool true], Value.Bool false)
            // (0, 3, f) + ((1, 2, t) * (3, 3, t))-> (3, 9, t)
            (Add [
                Tuple [Int 0; Int 3; Bool false]
                Multiply [
                    Tuple [Int 1; Int 2; Bool true]
                    Tuple [Int 3; Int 3; Bool true]]
            ], Value.Tuple [I 3; I 9; B true])

            // Adding tuples, is item-wise:
            // (0, 3, f) + (1, 2, t) (10, 20, f) -> (11, 25, t)
            (Add [
                Tuple [Int 0; Int 3; Bool false]
                Tuple [Int 1; Int 2; Bool true]
                Tuple [Int 10; Int 20; Bool false]
            ], Value.Tuple [I 11; I 25; B true])
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            // 0 + 
            (Add [Int 0], "Need at least two operands, received: 1")
            // *
            (Multiply [], "Need at least two operands, received: 0")
            // 0 + false
            (Add [Int 0; Bool false], "Invalid operands: 0 and False")
            // true + 1 
            (Add [Bool true; Int 1], "Invalid operands: True and 1")
            // (true, 1) + (true, true)
            (Add [
                Tuple [Bool true; Int 1]
                Tuple [Bool true; Bool false]
            ], "Cannot evaluate: tuple elements must have be of the same type: 1 != False")
            // (true, 1) + (2, 4)
            (Add [
                Tuple [Bool true; Int 1]
                Tuple [Int 2; Int 4]
            ], "Cannot evaluate: tuple elements must have be of the same type: True != 2")
            // (1, 1, 3) + (2, 4)
            (Add [
                Tuple [Int 1; Int 1; Int 3]
                Tuple [Int 2; Int 4]
            ], "Cannot evaluate: tuples must have the same size")
            // [1, 2] + (1, 2)
            (Add [
                Set [Int 1; Int 2]
                Tuple [Int 1; Int 2]
            ], "Invalid operands: [1, 2] and (1, 2)")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))



    [<TestMethod>]
    member this.BoolExpressions() =
        let ctx = this.Context

        [
            // true -> true
            (Bool(true), Value.Bool(true))
            // ! true -> false
            (Not(Bool(true)), Value.Bool(false))
            
            // true and true and true -> true
            (And([Bool true; Bool true; Bool true]), Value.Bool(true))
            // true and true and false -> false
            (And([Bool true; Bool true; Bool false]), Value.Bool(false))
            // false and true and true -> false
            (And([Bool false; Bool true; Bool true]), Value.Bool(false))
            // true and false and true -> false
            (And([Bool true; Bool false; Bool true]), Value.Bool(false))

            // true or true or true -> true
            (Or([Bool true; Bool true; Bool true]), Value.Bool(true))
            // true or false or false -> true
            (Or([Bool true; Bool false; Bool false]), Value.Bool(true))
            // false or false or true -> true
            (Or([Bool false; Bool false; Bool true]), Value.Bool(true))
            // false or false or false -> false
            (Or([Bool false; Bool false; Bool false]), Value.Bool(false))

            // 7 < 10 -> true
            (LessThan(Int 7, Int 10), Value.Bool(true))
            // 7 < 7 -> false
            (LessThan(Int 7, Int 7), Value.Bool(false))
            // 7 < 5 -> false
            (LessThan(Int 7, Int 5), Value.Bool(false))

            (Equals(
                Set [
                    Tuple [Bool false; Int 3; Int 5]
                    Tuple [Bool true; Int 13; Int 15]
                ],
                Set [
                    Tuple [Bool true; Int 13; Int 15]
                    Tuple [Bool false; Int 3; Int 5]
                ]
            ), Value.Bool(true))

            // true and false and 4
            (And [
                Bool true
                Bool false
                Int 4
            ], Value.Bool false)

            // i1 == 3 and ! false and (7) < 10 and (false, 5) == t1 and not ((1,2,3) == t1) -> true
            (And [
                Equals (Id("i1"), Int(3))
                Not (Bool(false))
                LessThan(Int 7, Int 10)
                Equals(Tuple [Bool false; Int 5], Id("t1"))
                Not(Equals(Tuple [Int 1; Int 2; Int 3], Id("t1")))
            ], Value.Bool(true))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            (Not (Int 4), "Invalid expression: !4")
            (And [
                Bool true
                Bool true
                Int 4
            ], "Invalid expression: True and 4")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))


    [<TestMethod>]
    member this.ProjectExpressions() =
        let ctx = this.Context

        [
            // (3,4,5).0 -> 3
            (Project(Tuple [Int(3); Int(4); Int 5], [Int 0]), Value.Int 3)
            // (3,4,5).1 -> 4
            (Project(Tuple [Int(3); Int(4); Int 5], [Int 1]), Value.Int 4)
            // (3,4,5).2 -> 5
            (Project(Tuple [Int(3); Int(4); Int 5], [Int 1]), Value.Int 4)
            // (3,4,5).[0,1] -> (3,4)
            (Project(Tuple [Int(3); Int(4); Int 5], [Int 0;Int 1]), Value.Tuple[I 3; I 4])
            // (3,4,5).[0,1,2] -> (3,4,5)
            (Project(Tuple [Int(3); Int(4); Int 5], [Int 0;Int 1; Int 2]), Value.Tuple[I 3; I 4; I 5])
            // (3,4,5).[1,1] -> (4,4)
            (Project(Tuple [Int(3); Int(4); Int 5], [Int 1;Int 1]), Value.Tuple[I 4; I 4])
            // (2,1).[0,0,0] -> (2,2,2)
            (Project(Tuple [Int 2;Int 1], [Int 0; Int 0; Int 0]), Value.Tuple[I 2;I 2; I 2])

            // [(3,4,5)].0 -> [3]
            (Project(Set [Tuple [Int(3); Int(4); Int 5]], [Int 0]),Value.Set (SET [[I 3]]))
            // [3,4,5].0 -> [3,4,5]
            (Project(Set [Int(3); Int(4); Int 5], [Int 0]),Value.Set (SET [[I 3];[I 4];[I 5]]))
            // [(1,2), (3,4), (5,6)).1 -> [2,4,6]
            (Project(Set [
                Tuple [Int 1; Int 2]
                Tuple [Int(3); Int(4)]
                Tuple [Int 5; Int 6]
            ], [Int 1]), Value.Set (SET [[I 2];[I 4];[I 6]]))
            // [(1,2,10), (3,4,11), (5,6,12)).[0,2] -> [(1,10),(3,11),(5,12)]
            (Project(Set [
                Tuple [Int 1; Int 2; Int 10]
                Tuple [Int(3); Int(4); Int 11]
                Tuple [Int 5; Int 6; Int 12]
            ], [Int 0; Int 2]), Value.Set (SET [[I 1;I 10];[I 3;I 11];[I 5;I 12]]))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            // 1.0
            (Project(Int 1, [Int 0]), "Unable to project from Int 1")
            // (2,1).3
            (Project(Tuple [Int 2;Int 1], [Int 3]), "Index in project outside of range")
            // (2,1).[0,3]
            (Project(Tuple [Int 2;Int 1], [Int 0; Int 3]), "Index in project outside of range")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))



    [<TestMethod>]
    member this.CallMethodExpressions() =
        let ctx = 
            this.Context
                .Add("void", Value.Method ([], Tuple []))
                .Add("colors", Value.Method ([], Tuple [Int 1;Int 2; Int 3]))
                .Add("toTuple", Value.Method (["a"; "b"; "c"], Tuple [Id "a";Id "b"; Id "c"]))
                .Add("shadow", Value.Method (["i1"], Id "i1"))
                .Add("shadow", Value.Method (["i1"], Block ([Let ("i1", Set [Int 0; Id "i1"])], Id "i1")))

        [
            // void() -> ()
            (CallMethod("void", []), Value.Tuple [])
            // colors() -> (1, 2, 3)
            (CallMethod("colors", []), Value.Tuple [I 1; I 2; I 3])
            // shadow(21) -> [0, 21]
            (CallMethod("shadow", [Int 21]), Value.Set (SET [[I 0]; [I 21]]))
            // toTuple(10, i1 * 10, 20 == 25) -> (10, 20, true)
            (CallMethod("toTuple", [
                Int 10
                Multiply[Id "i1"; Int 10]
                Equals (Int 20, Int 25)
            ]), Value.Tuple [I 10; I 30; B false])

            // given i1 = 3
            // given classic plusone (i1) -> i1 + 1
            // plusone(i1 + 7) -> 11
            (CallMethod("plusone", [Add [Id "i1"; Int 7]]), Value.Int 11)
            // plusone (10) -> 11
            (CallMethod("plusone", [ Int 10]), Value.Int 11)
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            (CallMethod("plusone", []), "Invalid arguments: expects 1, got 0")
            (CallMethod("plusone", [Int 1; Int 1]), "Invalid arguments: expects 1, got 2")
//            (CallMethod("plusone", [Id "k1"]), "Invalid operands: | (0, 1, 3, False, 5), (10, 11, 13, True, 25) > and 1")
            (CallMethod("plusone", [Tuple [Int 3;  Int 10]]), "Invalid operands: (3, 10) and 1")
            (CallMethod("foo", [Id "t1"]), "Undefined method: foo")
            (CallMethod("t1", [Id "i1"]), "Undefined method: t1")

        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))




    [<TestMethod>]
    member this.ForExpressions() =
        let ctx = 
            this.Context
                .Add("sum", Value.Method (["a"; "b"], Add [Id "a"; Id "b"]))
                .Add("times", Value.Method (["a"; "b"], Multiply [Id "a"; Id "b"]))
                .Add("and", Value.Method (["a"; "b"], And [Id "a"; Id "b"]))
                .Add("or", Value.Method (["a"; "b"], Or [Id "a"; Id "b"]))

        let t = Tuple [Int 1; Int 2; Int 3; Int 4]
        let s = Set [Int 1; Int 2; Int 3; Int 4]
        let s5 = Set [Int 5; Int 10; Int 15]

        [
            // summarize e in [1;2;3;4] with sum e -> 10
            Summarize ("e", s, "sum", Id "e"), Value.Int 10

            // summarize e in [1;2;3;4] with and { let i = e; i < 10 } -> true
            Summarize ("e", t, "and", Block ([
                Let ("i", Id "e")],
                LessThan (Id "i", Int 10))), Value.Bool true

            // summarize e in [5;10;15] with and { let i = e; i < 10 } -> false
            Summarize ("e", s5, "and", Block ([
                Let ("i", Id "e")],
                LessThan (Id "i", Int 10))), Value.Bool false

            // summarize e in [5;10;15] with or { e < 10 } -> true
            Summarize ("e", s5, "or", LessThan (Id "e", Int 119)), Value.Bool true

            // summarize e in 21 with sum e -> 21
            Summarize ("x", Int 21, "sum", Id "x"), Value.Int 21

            // summarize e in true with or e -> trye
            Summarize ("e", Bool true, "or", Id "e"), Value.Bool true

            // summarize e in (false, false, true) with sum e -> true
            Summarize ("e", Tuple [Bool false; Bool false; Bool true], "sum", Id "e"), Value.Bool true
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            Summarize ("e", s5, "or", LessThan (Id "i", Int 119)), "Unknown variable: i"
            Summarize ("e", Tuple [Bool false; Int 3], "sum", Id "e"), "Invalid operands: False and 3"
            Summarize ("e", s5, "foo", Id "e"), "Undefined method: foo"
            Summarize ("e", Id "plusone", "or", LessThan (Id "i", Int 119)), "Invalid enumeration: (a) -> ()"
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))




    [<TestMethod>]
    member this.LetStmt() =
        let ctx = this.Context

        let testValid (e, key, expected) =
            match eval (Block ([e], Id key), ctx) with
            | Error msg -> Assert.AreEqual("", $"Error on key '{key}': {msg}")
            | Ok (actual, _) -> Assert.AreEqual(expected.ToString(), actual.ToString())

        let testInvalid (e, (key: string), expected) =
            match eval (Block ([e], Id key), ctx) with
            | Error msg -> Assert.AreEqual(expected, msg)
            | Ok (_, ctx) ->
                let actual = ctx.Map[key]
                Assert.AreEqual(expected, actual)


        [
            (Let("a", Int(7)), "a", Value.Int 7)
            (Let("b", Tuple([Int(3); Int(5)])), "b", Value.Tuple([I(3); I(5)]))
            (Let("c", Block([], Id("b1"))), "c", Value.Tuple([B false]))
            (Let("d", Block([Let ("i1", Int 25)], Id "i1")), "d", Value.Int 25)
            (Let("d", Block([Let ("i1", Int 25)], Id "i1")), "i1", Value.Int 3)
            (Let("e", 
                Block ([
                    Let ("a", Int 0)
                    Let ("a", Int 1)
                    Let ("a", Int 2)], 
                    Multiply [Int 10; Id "a"])), 
                "e", Value.Int 20)
            (Let("f", 
                Block ([
                    Let ("a", Int 0)
                    Let ("b", Int 1)
                    Let ("c", Int 2)],
                    Add [Id "a"; Id "c"])),
                "f", Value.Int 2)
            (Let("g", CallMethod("plusone", [Add [Id "i1"; Int 7]])), "g", Value.Int 11)
        ]
        |> List.iter testValid

        // Make sure errors are correctly reported:
        [
            Let("alpha", Id("invalid")), "invalid", "Unknown variable: invalid"
            Let("beta", Block([Let ("internal", Int 25)], Id "internal")), "internal", "Unknown variable: internal"
        ]
        |> List.iter testInvalid


    [<TestMethod>]
    member this.IfExpressions() =
        let ctx = this.Context
            
        [
            (If(Bool true,
                    Int -1,
                    Int 1), 
                Value.Int -1)

            (If(Bool false,
                    Int -1,
                    Int 1), 
                Value.Int 1)
        ]
        |> List.iter this.TestExpression

        // Make sure errors are correctly reported:
        [
            If (Int 4, Int -1, Int 1), "Invalid condition: 4"
        ]
        |> List.iter this.TestInvalidExpression

            
