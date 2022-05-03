namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.compiler
open aleph.runtime.Classic

[<TestClass>]
type TestClassic () =

    member this.TestExpression (e, expected, ?ctx)=
        let ctx = defaultArg ctx Map.empty
        match eval (e, ctx) with
        | Ok (actual, _) -> 
            Assert.AreEqual(expected, actual)
        | Result.Error msg -> 
            Assert.AreEqual(expected, msg)
            Assert.Fail()
        
    member this.TestInvalidExpression (e, expected, ?ctx)=
        let ctx = defaultArg ctx Map.empty
        match eval (e, ctx)  with
        | Result.Error actual -> 
            Assert.AreEqual(expected, actual)
        | Result.Ok (v, _) ->
            Assert.AreEqual($"ERROR for {e}", $"Expression returned: {v}")

    member this.Context =
        let i1 = Value.Tuple [I 3]
        let b1 = Value.Tuple [B false]
        let t1 = Value.Tuple [B false; I 5]
        let t2 = Value.Tuple [B true; I 12]
        let s1 = Value.Set(SET [
            [B false; I 5]
            [B true; I 12]
        ])
        let k1 = Value.Ket( SET [
            [I  0; I  1; I  3; B false; I  5]
            [I 10; I 11; I 13; B true;  I 25]
        ])
        // classic = plusone (a) -> a + 1
        let plusone = Value.Classic (["a"], Return (Add([Id "a"; Int 1])))

        Map[ 
            ("i1", i1)
            ("b1", b1)
            ("t1", t1)
            ("t2", t2)
            ("s1", s1)
            ("k1", k1)
            ("plusone", plusone)
        ]


    [<TestMethod>]
    member this.LiteralExpressions() =
        this.TestExpression(Int(5), Value.Int 5)
        this.TestExpression(Bool(true), Value.Bool true)
        this.TestExpression(Bool(false), Value.Bool false)


    [<TestMethod>]
    member this.IdExpressions() =
        let ctx = this.Context

        for i in ctx.Keys do
            this.TestExpression(Id(i), ctx.[i], ctx)

        // Some negative cases too:
        this.TestInvalidExpression(Id("i1"), "Unknown variable: i1", Map.empty)

        Assert.IsFalse(ctx.ContainsKey "foo")
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
            (Set([Id("s1")]), ctx["s1"])            // Tuple with set inside

            //  given s1 = [ (F, 5), (T,12) ]
            // [ s1, s1 ] -> [ (F, 5), (T,12) ]
            (Set([Id("s1");Id("s1")]), ctx["s1"])            // Tuple with set inside

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
            //[ | 1, 2> ] : Set of kets
            (Set [
                Ket [
                    Int(1)
                    Int(2) ]
            ], "Invalid value for a set element: | 1, 2 >")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))


    [<TestMethod>]
    member this.KetExpressions() =
        let ctx = this.Context

        // ---------------------------------- //
        // Positive cases
        // ---------------------------------- //
        [
            // |>
            (Ket([]), Value.Ket(SET []))
            
            // |[], []> -> |>
            (Ket([
                Set([])
                Set([])
            ]), Value.Ket(SET []))

            
            // |(3, 4)> -> |(3, 4)>
            (Ket([
                Tuple([Int(3); Int(4)])
            ]), Value.Ket(SET [[I 3; I 4]]))

            // |0> -> |0>
            (Ket([Int(0)]), Value.Ket(SET[
                [I 0]
            ]))
            
            // |1, 2, 1> --> |(1), (2)>
            (Ket([Int(1); Int(2); Int(1)]), Value.Ket(SET[
                [I 1]
                [I 2]
            ]))
            
            //| [ (1,t), (3,f) ] > --> | (1,t), (3,f) >
            (Ket [
                Set [
                    Tuple[Int(1); Bool(true)]
                    Tuple[Int(3); Bool(false)]]
            ], Value.Ket(SET [
                [I 1 ; B true]
                [I 3 ; B false]
            ]))

            // | s1 > --> | (5, 12) >
            (Ket [ Id "s1"]), Value.Ket(SET[
                [B false; I 5]
                [B true; I 12]
            ])

            // ( | (0,0,0), (1,1,1) >, | (0,1,0), (1,1,1) >  ) --> | (0,0,0,0,1,0), (0,0,0,0,1,0), (1,1,1,0,1,0), (1,1,1,1,1,1) >
            (Tuple([
                Ket([
                    Tuple([Int(0); Int(0); Int(0)])
                    Tuple([Int(1); Int(1); Int(1)])
                ])
                Ket([
                    Tuple([Int(0); Int(1); Int(0)])
                    Tuple([Int(1); Int(1); Int(1)])
                ])
            ]), Value.Ket(SET([
                [I(0); I(0); I(0); I(0); I(1); I(0)]
                [I(0); I(0); I(0); I(1); I(1); I(1)]
                [I(1); I(1); I(1); I(0); I(1); I(0)]
                [I(1); I(1); I(1); I(1); I(1); I(1)]
            ])))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))


        // ---------------------------------- //
        // Negative cases
        // ---------------------------------- //
        [
            //  given k1 = | (F, 5), (T,12) >
            // | k1 >
            (Ket [Id("k1")], "Invalid value for a set element: | (0, 1, 3, False, 5), (10, 11, 13, True, 25) >")

            //| | 1, 2> >
            (Ket [
                Ket [
                    Int(1)
                    Int(2) ]
            ], "Invalid value for a set element: | 1, 2 >")
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
            // | 0..3 > -> |0, 1, 2>
            (Ket [Range(Int(0), Int(3))], Value.Ket (SET [[I 0]; [I 1]; [I 2]]))
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
            (Equals(
                Set [Bool false; Int 3],
                Ket [Bool false; Int 3]
            ), "Invalid expression: [False, 3] == | False, 3 >")
            (Or [
                Ket [Bool false; Int 3]
                Ket [Bool false; Int 3]
            ], "Invalid expression: | False, 3 > or | False, 3 >")
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
    member this.CallClassicExpressions() =
        let ctx = 
            this.Context
                .Add("void", Value.Classic ([], Skip))
                .Add("colors", Value.Classic ([], Return (Tuple[Int 1;Int 2; Int 3])))
                .Add("toTuple", Value.Classic (["a"; "b"; "c"], Return (Tuple[Id "a";Id "b"; Id "c"])))
                .Add("shadow", Value.Classic (["i1"], Return (Id "i1")))

        [
            // void() -> ()
            (CallClassic("void", []), Value.Tuple [])
            // colors() -> (1, 2, 3)
            (CallClassic("colors", []), Value.Tuple [I 1; I 2; I 3])
            // shadow(21) -> 21
            (CallClassic("shadow", [Int 21]), Value.Int 21)
            // toTuple(10, i1 * 10, 20 == 25) -> (10, 20, true)
            (CallClassic("toTuple", [
                Int 10
                Multiply[Id "i1"; Int 10]
                Equals (Int 20, Int 25)
            ]), Value.Tuple [I 10; I 30; B false])

            // given i1 = 3
            // given classic plusone (i1) -> i1 + 1
            // plusone(i1 + 7) -> 11
            (CallClassic("plusone", [Add [Id "i1"; Int 7]]), Value.Int 11)
            // plusone (10) -> 11
            (CallClassic("plusone", [ Int 10]), Value.Int 11)
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            (CallClassic("plusone", []), "Invalid arguments: expects 1, got 0")
            (CallClassic("plusone", [Int 1; Int 1]), "Invalid arguments: expects 1, got 2")
            (CallClassic("plusone", [Id "k1"]), "Invalid operands: | (0, 1, 3, False, 5), (10, 11, 13, True, 25) > and 1")
            (CallClassic("plusone", [Tuple [Int 3;  Int 10]]), "Invalid operands: (3, 10) and 1")
            (CallClassic("foo", [Id "t1"]), "Undefined classic: foo")
            (CallClassic("t1", [Id "i1"]), "Undefined classic: t1")

        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))


    [<TestMethod>]
    member this.CallQuantumExpressions() =
        let ctx = 
            this.Context
                .Add("void", Value.Quantum ([], "k", Skip))
                .Add("colors", Value.Quantum ([], "k", Return (Tuple[Int 1;Int 2; Int 3])))
                .Add("append", Value.Quantum (["a"], "k", Return (Id "a")))
                .Add("shadow", Value.Quantum (["i1"], "k1", Return (Add [Id "k1"; Id "i1"])))
                .Add("qone", Value.Quantum ([], "k1", Return (CallClassic ("plusone", [Project (Id "k1", [Int 0])]))))
                //TODO: .Add("last", Value.Quantum ([], "k1", Return (Project (Id "k1", [Int -1]))))

        let empty = Ket []
        let k1 = Ket [Int 0]
        let k3 = Ket [Int 0; Int 1; Int 2]
        let tuples = Ket [
            Tuple [Int 0; Bool true]
            Tuple [Int 1; Bool false]
            Tuple [Int 2; Bool true]
        ]
        let triples = Ket [
            Tuple [Int 0; Int 0; Bool true]
            Tuple [Int 1; Int 1; Bool false]
            Tuple [Int 2; Int 2; Bool true]
        ]

        [
            // void() |> -> ()
            (CallQuantum("void", [], empty), Value.Ket (SET []))
            // void() k1 -> |0>
            (CallQuantum("void", [], k1), Value.Ket (SET [[I 0]]))
            
            // colors() empty -> |>
            (CallQuantum("colors", [], empty), Value.Ket (SET []))
            // colors() k1 -> |(0, 1, 2, 3)>
            (CallQuantum("colors", [], k1), Value.Ket (SET [[I 0; I 1; I 2; I 3]]))
            // colors() k3 -> |(0,1, 2, 3), (1, 1, 2, 3), (2, 1, 2, 3)>
            (CallQuantum("colors", [], k3), Value.Ket (SET [
                [I 0; I 1; I 2; I 3]
                [I 1; I 1; I 2; I 3]
                [I 2; I 1; I 2; I 3]
            ]))
            (CallQuantum("colors", [], tuples), Value.Ket (SET [
                [I 0; B true;  I 1; I 2; I 3]
                [I 1; B false; I 1; I 2; I 3]
                [I 2; B true;  I 1; I 2; I 3]
            ]))

            
            // shadow(10) k3 -> | (0,10), (1,11), (2,12) >
            (CallQuantum("shadow", [Int 10], k3), Value.Ket (SET [[I 0; I 10]; [I 1; I 11]; [I 2; I 12]]))

            // append(true) empty -> |>
            (CallQuantum("append", [Bool true], empty), Value.Ket (SET []))
            // append( (false, false) ) k1 -> |(0, false, false)>
            (CallQuantum("append", [Tuple [Bool false; Bool false]], k1), Value.Ket (SET [
                [I 0; B false; B false]
            ]))
            // append( (false, false) ) k3 -> |(0, false, false), (1, false, false), (2, false, false)>
            (CallQuantum("append", [Tuple [Bool false; Bool false]], k3), Value.Ket (SET [
                [I 0; B false; B false]
                [I 1; B false; B false]
                [I 2; B false; B false]
            ]))
            // append( [false, false, true] ) tuples -> |(0,true,false), (0,true,true), (1,false,false), (1,false, true), (2,true,false) (2,true,true)>
            (CallQuantum("append", [Set [Bool false; Bool false; Bool true]], tuples), Value.Ket (SET [
                [I 0; B true; B false]
                [I 0; B true; B true]
                [I 1; B false; B false]
                [I 1; B false; B true]
                [I 2; B true; B false]
                [I 2; B true; B true]
            ]))

            // qone() k3 -> | (0,1), (1,2), (2,3) >
            (CallQuantum("qone", [], k3), Value.Ket (SET [[I 0; I 1]; [I 1; I 2]; [I 2; I 3]]))

            // qone() tuples -> | (0,true,1), (1,false,2), (2,true,3) >
            (CallQuantum("qone", [], k3), Value.Ket (SET [[I 0; I 1]; [I 1; I 2]; [I 2; I 3]]))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            (CallQuantum("append", [], empty), "Invalid arguments: expects 1, got 0")
            (CallQuantum("append", [Int 1; Int 1], empty), "Invalid arguments: expects 1, got 2")
            (CallQuantum("foo", [Id "t1"], empty), "Undefined quantum: foo")
            (CallQuantum("t1", [Id "i1"], empty), "Undefined quantum: t1")
            (CallQuantum("append", [Id "i1"], Int 777), "Expecting ket value, got: 777")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))


    [<TestMethod>]
    member this.SolveExpressions() =
        let ctx = this.Context

        [
            // Solve |(3, true), (4, false), (5, false))> -> |3>
            (Solve(Ket [
                Tuple [Int 3; Bool true]
                Tuple [Int 4; Bool false]
                Tuple [Int 5; Bool false]
            ]), Value.Ket (SET [ [I 3] ]))
            // Solve |(3, true), (4, true), (5, true))> -> |3, 4, 5>
            (Solve(Ket [
                Tuple [Int 3; Bool true]
                Tuple [Int 4; Bool true]
                Tuple [Int 5; Bool true]
            ]), Value.Ket (SET [ [I 3]; [I 4]; [I 5] ]))
            // Solve |(1, 1, true), (2, 4, false), (3, 5, true))> -> |(1, 1), (1, 5)>
            (Solve (Ket [
                Tuple [Int 1; Int 1; Bool true]
                Tuple [Int 2; Int 4; Bool false]
                Tuple [Int 3; Int 5; Bool true]
            ]), Value.Ket (SET [ [I 1; I 1]; [I 3; I 5] ]))
            // Solve |(1, 1, 13, 0) (2, 4, 24, -1), (3, 5, 35, 1))> -> |(2, 4, 24)>
            (Solve (Ket [
                Tuple [Int 1; Int 1; Int 13; Int 0]
                Tuple [Int 2; Int 4; Int 24; Int -1]
                Tuple [Int 3; Int 5; Int 35; Int 10]
                Tuple [Int 3; Int 5; Int 35; Int 12]
                Tuple [Int 2; Int 4; Int 24; Int -1]
                Tuple [Int 3; Int 5; Int 12; Int 0]
                Tuple [Int 3; Int 5; Int 25; Int -1]
            ]), Value.Ket (SET [ [I 2; I 4; I 24]; [I 3; I 5; I 25] ]))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            // Solve 1
            (Solve(Int 1), "Solve not available for 1")
            // Solve |-1,4,5>
            (Solve(Ket [Int -1; Int 4; Int 5]), "Solve expects kets of size > 2. Ket size: 1")

        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))

    [<TestMethod>]
    member this.ReturnStmt() =
        let ctx = this.Context

        let testOne (stmt, expected) =
            match run (stmt, ctx) with
            | Continue _ -> Assert.AreEqual("", "Statement returned void.")
            | Fail (msg, _) -> Assert.AreEqual("", $"Error on stmt '{stmt}': {msg}")
            | Result (actual, _) -> Assert.AreEqual(expected, actual)
            
        [
            (Return(Int(7)), Value.Int 7)
            (Return(Tuple([Int(3); Int(5)])), Value.Tuple([I(3); I(5)]))
            (Block([Return(Id("b1"))]), Value.Tuple([B false]))
            (Block([
                Skip
                Return(Id("t1"))
            ]),  Value.Tuple([B(false); I(5)]))
            (Block([
                Skip
                Return(Id("i1"))
                Return(Id("b1"))
            ]),  Value.Tuple([I 3]))
        ]
        |> List.iter testOne

        // Make sure errors are correctly reported:
        let invalid = Return(Id("foo"))
        match run (invalid, Map.empty) with
        | Fail (msg, _) -> Assert.AreEqual("Unknown variable: foo", msg)
        | Continue _ -> Assert.AreEqual("", "Statement returned void.")
        | Result (actual, _) -> Assert.AreEqual("", $"Statement returned {actual}. Expecting error.")


    [<TestMethod>]
    member this.LetStmt() =
        let ctx = this.Context

        let testOne (stmt, (key, expected)) =
            match run (stmt, ctx) with
            | Fail (msg, _) -> Assert.AreEqual("", $"Error on key '{key}': {msg}")
            | Result (actual, _) -> Assert.AreEqual("", $"Statement return Result: {actual}.")
            | Continue ctx -> 
                let actual = ctx[key]
                Assert.AreEqual(expected, actual)

        [
            (Let("a", Int(7)), ("a", Value.Int 7))
            (Let("b", Tuple([Int(3); Int(5)])), ("b", Value.Tuple([I(3); I(5)])))
            (Block([Let("c", Id("b1"))]), ("c", Value.Tuple([B false])))
            (Block([
                Skip
                Let("d", Id("t1"))
            ]), ("d", Value.Tuple([B(false); I(5)])))
            (Block([
                Skip
                Let("e", Id("b1"))
                Let("e", Id("i1"))
            ]),  ("e", Value.Tuple([I 3])))
            (Block([
                Skip
                Let("f", Id("t1"))
                Skip
            ]), ("f", Value.Tuple([B(false); I(5)])))
            (Block([
                Skip
                Let("g1", Id("b1"))
                Let("g2", Int(23))
                Let("g3", Id("i1"))
            ]),  ("g2", Value.Int 23))
        ]
        |> List.iter testOne

        // Make sure errors are correctly reported:
        let invalid = Let("foo", Id("foo"))
        match run (invalid, Map.empty) with
        | Fail (msg, _) -> Assert.AreEqual("Unknown variable: foo", msg)
        | Continue _ -> Assert.AreEqual("", "Statement returned void.")
        | Result (actual, _) -> Assert.AreEqual("", $"Statement returned {actual}. Expecting error.")


    [<TestMethod>]
    member this.IfStmt() =
        let ctx = this.Context

        let testOne (stmt, expected) =
            match run (stmt, ctx) with
            | Continue _ -> Assert.AreEqual("", "Statement returned void.")
            | Fail (msg, _) -> Assert.AreEqual("", $"Error on stmt '{stmt}': {msg}")
            | Result (actual, _) -> Assert.AreEqual(expected, actual)
            
        [
            (If(Bool true,
                    Return (Int -1),
                    Return (Int 1)), 
                Value.Int -1)

            (If(Bool false,
                    Return (Int -1),
                    Return (Int 1)), 
                Value.Int 1)
        ]
        |> List.iter testOne

        // Make sure errors are correctly reported:
        let invalid = If (Int 4, Return (Int -1),Return (Int 1))
        match run (invalid, Map.empty) with
        | Fail (msg, _) -> Assert.AreEqual("Invalid condition: Int 4", msg)
        | Continue _ -> Assert.AreEqual("", "Statement returned void.")
        | Result (actual, _) -> Assert.AreEqual("", $"Statement returned {actual}. Expecting error.")


    [<TestMethod>]
    member this.CallClassicStmt() =
        let ctx = this.Context

        let program = Block [
            DefClassic ("echo", ["v1"], (Return (Id "v1")))
            Return (CallClassic ("echo", [Int 5]))
        ]

        match run (program,Map.empty) with
        | Continue _ -> Assert.AreEqual("", "Statement returned void.")
        | Fail (msg, _) -> Assert.AreEqual("", $"Error on stmt 'echo': {msg}")
        | Result (actual, _) -> Assert.AreEqual(Value.Int 5, actual)
            

    [<TestMethod>]
    member this.CallQuantumStmt() =
        let ctx = this.Context

        let program = Block [
            // let k1 = |1, 5, 10>
            Let("k1", Ket [Int 1; Int 5; Int 10])
            // classic echo (v1) -> v1
            DefClassic ("echo", ["v1"], (Return (Id "v1")))
            // quantum qecho (v3) k -> (v3, k)
            DefQuantum ("qecho", ["v3"], "k", (Return (Tuple [Id "v3"; Id "k"])))
            // qecho (false) k1 -> | (false, 1), (false, 5), (false, 10) >
            Return (CallQuantum ("qecho", [Bool false], (Id "k1")))
        ]
        let expected = Value.Ket (SET [ 
            [I 1; B false; I 1]
            [I 5; B false; I 5]
            [I 10; B false; I 10]
        ])
        
        match run (program,Map.empty) with
        | Continue _ -> Assert.AreEqual("", "Statement returned void.")
        | Fail (msg, _) -> Assert.AreEqual("", $"Error on stmt 'echo': {msg}")
        | Result (actual, _) -> Assert.AreEqual(expected, actual)
            
