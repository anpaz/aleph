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
        let plusone = Value.Classic (["a"], ast.Return (ast.Add([ast.Id "a"; ast.Int 1])))

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
        this.TestExpression(ast.Int(5), Value.Int 5)
        this.TestExpression(ast.Bool(true), Value.Bool true)
        this.TestExpression(ast.Bool(false), Value.Bool false)


    [<TestMethod>]
    member this.IdExpressions() =
        let ctx = this.Context

        for i in ctx.Keys do
            this.TestExpression(ast.Id(i), ctx.[i], ctx)

        // Some negative cases too:
        this.TestInvalidExpression(ast.Id("i1"), "Unknown variable: i1", Map.empty)

        Assert.IsFalse(ctx.ContainsKey "foo")
        this.TestInvalidExpression(ast.Id("foo"), "Unknown variable: foo", ctx)


    [<TestMethod>]
    member this.TuplesExpressions() =
        let ctx = this.Context

        [
            // () -> ()
            (ast.Tuple([]), Value.Tuple([]))
            // (3) -> (3)
            (ast.Tuple([ast.Int(3)]), Value.Tuple([I(3)]))
            // (3,5) -> (3,5)
            (ast.Tuple([ast.Int(3); ast.Int(5)]), Value.Tuple([I(3); I(5)]))
            // (f, b1, t) -> (f, f, t)
            (ast.Tuple([ast.Bool(false); ast.Id("b1"); ast.Bool(true)]), Value.Tuple([B(false); B(false); B(true)]))
            // (i1, i1) -> (3, 3)
            (ast.Tuple([ast.Id("i1"); ast.Id("i1")]), Value.Tuple([I 3; I 3]))
            // ((3)) --> (3)
            (ast.Tuple([ast.Tuple([ast.Int(3)])]), Value.Tuple([I(3)]))
            // ((0,1)) --> (0,1)
            (ast.Tuple([ast.Tuple([ast.Int(0); ast.Int(1)])]), Value.Tuple([I(0); I(1)]))
            // ((1,2),(3,4)) = (1,2,3,4)
            (ast.Tuple([
                    ast.Tuple([ast.Int(1); ast.Int(2)])
                    ast.Tuple([ast.Int(3); ast.Int(4)])]),
                Value.Tuple([I(1); I(2); I(3); I(4)]))
            // ((0,1), 2, ((3, 4), 5)) --> (0,1,2,3,4,5)
            (ast.Tuple([
                    ast.Tuple([ast.Int(0); ast.Int(1)])
                    ast.Int(2)
                    ast.Tuple([
                        ast.Tuple([ast.Int(3); ast.Int(4)])
                        ast.Int(5)
                    ])]),
                Value.Tuple([I(0); I(1); I(2); I(3); I(4); I(5)]))
            // ([0,1], 2) -> [(0,2), (1,2)]
            (ast.Tuple([
                    ast.Set([ast.Int(0); ast.Int(1)])
                    ast.Int(2)]), 
                Value.Set(SET [
                    [I(0); I(2)]
                    [I(1); I(2)]]))
            // ( [ (0,1,2), (3,4,5) ], [ (6,F), (7,T) ] ) -> [(0,1,2,6,F), (0,1,2,7,T), (3,4,5,6,F), (3,4,5,7,T)]
            (ast.Tuple([
                    ast.Set([
                        ast.Tuple([ast.Int(0); ast.Int(1); ast.Int(2)])
                        ast.Tuple([ast.Int(3); ast.Int(4); ast.Int(5)])
                    ])
                    ast.Set([
                        ast.Tuple([ast.Int(6); ast.Bool(false)])
                        ast.Tuple([ast.Int(7); ast.Bool(true)])])
                ]), Value.Set(SET [
                        [I(0); I(1); I(2); I(6); B(false)]
                        [I(0); I(1); I(2); I(7); B(true)]
                        [I(3); I(4); I(5); I(6); B(false)]
                        [I(3); I(4); I(5); I(7); B(true)]
            ]))
            // ( s1 ) -> [ (F, 5), (T, 12) ]
            (ast.Tuple([ast.Id("s1")]), Value.Set(SET([
                [B(false); I(5)]
                [B(true); I(12)]
            ])))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            // TODO: this should fail: ( [1,2,3] )
            //ast.Tuple [ ast.Set [ast.Int 1; ast.Int 2; ast.Int 3] ]
            // Tuple with invalid id
            (ast.Tuple [ ast.Id("foo") ], "Unknown variable: foo")
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
            //(ast.Set([]), Value.Set(SET([])))
            
            // [[], []] -> []
            (ast.Set([
                ast.Set([])
                ast.Set([])
            ]), Value.Set(SET([])))

            
            // [[], (3, 4) []] -> [(3, 4)]
            (ast.Set([
                ast.Set([])
                ast.Tuple([ast.Int(3); ast.Int(4)])
                ast.Set([])
            ]), Value.Set(SET([[I 3; I 4]])))

            // [0] -> [(0)]
            (ast.Set([ast.Int(0)]), Value.Set(SET([
                [I(0)]
            ])))

            // [0, (1)] -> [(0), (1)]
            (ast.Set [
                ast.Int(0)
                ast.Tuple([ast.Int(1)])
            ], Value.Set(SET [
                [I(0)]
                [I(1)]
            ]))
            
            
            // [1, 2, 1] --> [(1), (2)]
            (ast.Set([ast.Int(1); ast.Int(2); ast.Int(1)]), Value.Set(SET([
                [I(1)]
                [I(2)]
            ])))
            
            //[(1,t), (3,f)] --> [(1,t), (3,f)]
            (ast.Set([
                ast.Tuple[ast.Int(1); ast.Bool(true)]
                ast.Tuple[ast.Int(3); ast.Bool(false)]
            ]), Value.Set(SET([
                [I(1); B(true)]
                [I(3); B(false)]
            ])))
            
            // given t1 = (f, 5)
            // [ t1 ] --> [ (f, 5) ]  
            (ast.Set([ast.Id("t1")]), Value.Set(SET([
                [B(false); I(5)]
            ])))

            // given t1 = (f, 5)
            // [ t1; t1 ] --> [ (f, 5) ]
            (ast.Set([ast.Id("t1");ast.Id("t1")]), Value.Set(SET([
                [B(false); I(5)]
            ])))
            
            // [ t1, (T, 12) ] --> [ (f, 5), (T, 12) ]
            (ast.Set([
                ast.Id("t1")
                ast.Tuple([ast.Bool(true); ast.Int(12)])
            ]), Value.Set(SET([
                [B(false); I(5)]
                [B(true); I(12)]
            ])))
            
            //  given s1 = [ (F, 5), (T,12) ]
            // [ s1 ] -> [ (F, 5), (T,12) ]
            (ast.Set([ast.Id("s1")]), ctx["s1"])            // Tuple with set inside

            //  given s1 = [ (F, 5), (T,12) ]
            // [ s1, s1 ] -> [ (F, 5), (T,12) ]
            (ast.Set([ast.Id("s1");ast.Id("s1")]), ctx["s1"])            // Tuple with set inside

            // [ [ (0,0), (1,1) ], (0,1), (1,1)  ] --> [ (0,0), (0,1), (1,1) ] ]
            (ast.Set([
                ast.Set([
                    ast.Tuple([ast.Int(0); ast.Int(0)])
                    ast.Tuple([ast.Int(1); ast.Int(1)])
                ])
                ast.Tuple([ast.Int(0); ast.Int(1)])
                ast.Tuple([ast.Int(1); ast.Int(1)])
            ]), Value.Set(SET([
                [I(0); I(0)]
                [I(0); I(1)]
                [I(1); I(1)]
            ])))

            // [ [ (0,0,0), (1,1,1) ], [(0,1,0), (1,1,1)]  ] --> [ (0,0,0), (0,1,0), (1,1,1) ]
            (ast.Set([
                ast.Set([
                    ast.Tuple([ast.Int(0); ast.Int(0); ast.Int(0)])
                    ast.Tuple([ast.Int(1); ast.Int(1); ast.Int(1)])
                ])
                ast.Set([
                    ast.Tuple([ast.Int(0); ast.Int(1); ast.Int(0)])
                    ast.Tuple([ast.Int(1); ast.Int(1); ast.Int(1)])
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
            (ast.Set [
                ast.Int(1)
                ast.Int(2)
                ast.Tuple([ ast.Int(2); ast.Int(3) ])
            ], "All tuples must have the same length. [1] != [2; 3]")
            //[1, (2, 3)] : Uneven lenghts
            (ast.Set [
                ast.Int(1)
                ast.Tuple([ast.Int(2);ast.Int(3)]) 
            ], "All tuples must have the same length. [1] != [2; 3]")
            //[ [(0,0), (1,2)], [(2, 3, 4)] ] : Uneven embeded lenghts
            (ast.Set [
                ast.Set [
                    ast.Tuple([ast.Int(0); ast.Int(0)])
                    ast.Tuple([ast.Int(1); ast.Int(2)])
                ]
                ast.Set[
                    ast.Tuple([ast.Int(2); ast.Int(3); ast.Int(4)])
                ]
            ], "All tuples must have the same length. [0; 0] != [2; 3; 4]")
            //[ | 1, 2> ] : Set of kets
            (ast.Set [
                ast.Ket [
                    ast.Int(1)
                    ast.Int(2) ]
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
            (ast.Ket([]), Value.Ket(SET []))
            
            // |[], []> -> |>
            (ast.Ket([
                ast.Set([])
                ast.Set([])
            ]), Value.Ket(SET []))

            
            // |(3, 4)> -> |(3, 4)>
            (ast.Ket([
                ast.Tuple([ast.Int(3); ast.Int(4)])
            ]), Value.Ket(SET [[I 3; I 4]]))

            // |0> -> |0>
            (ast.Ket([ast.Int(0)]), Value.Ket(SET[
                [I 0]
            ]))
            
            // |1, 2, 1> --> |(1), (2)>
            (ast.Ket([ast.Int(1); ast.Int(2); ast.Int(1)]), Value.Ket(SET[
                [I 1]
                [I 2]
            ]))
            
            //| [ (1,t), (3,f) ] > --> | (1,t), (3,f) >
            (ast.Ket [
                ast.Set [
                    ast.Tuple[ast.Int(1); ast.Bool(true)]
                    ast.Tuple[ast.Int(3); ast.Bool(false)]]
            ], Value.Ket(SET [
                [I 1 ; B true]
                [I 3 ; B false]
            ]))

            // | s1 > --> | (5, 12) >
            (ast.Ket [ ast.Id "s1"]), Value.Ket(SET[
                [B false; I 5]
                [B true; I 12]
            ])

            // ( | (0,0,0), (1,1,1) >, | (0,1,0), (1,1,1) >  ) --> | (0,0,0,0,1,0), (0,0,0,0,1,0), (1,1,1,0,1,0), (1,1,1,1,1,1) >
            (ast.Tuple([
                ast.Ket([
                    ast.Tuple([ast.Int(0); ast.Int(0); ast.Int(0)])
                    ast.Tuple([ast.Int(1); ast.Int(1); ast.Int(1)])
                ])
                ast.Ket([
                    ast.Tuple([ast.Int(0); ast.Int(1); ast.Int(0)])
                    ast.Tuple([ast.Int(1); ast.Int(1); ast.Int(1)])
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
            (ast.Ket [ast.Id("k1")], "Invalid value for a set element: | (0, 1, 3, False, 5), (10, 11, 13, True, 25) >")

            //| | 1, 2> >
            (ast.Ket [
                ast.Ket [
                    ast.Int(1)
                    ast.Int(2) ]
            ], "Invalid value for a set element: | 1, 2 >")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))


    [<TestMethod>]
    member this.RangeExpressions() =
        let ctx = this.Context

        [
            // 0..0 -> []
            (ast.Range(ast.Int(0), ast.Int(0)), Value.Set(SET []))
            // 0..3 -> [0, 1, 2]
            (ast.Range(ast.Int(0), ast.Int(3)), Value.Set (SET [[I 0]; [I 1]; [I 2]]))
            // i1..4 --> [3]
            (ast.Range(ast.Id("i1"), ast.Int(4)), Value.Set(SET [[I 3]]))
            // TODO: this feels wrong: ( 0..i1 ) -> [0, 1, 2]
            (ast.Tuple [ast.Range(ast.Int(0), ast.Id("i1"))], Value.Set (SET [[I 0]; [I 1]; [I 2]]))
            // [ 0..3 ] -> [0, 1, 2]
            (ast.Set [ast.Range(ast.Int(0), ast.Int(3))], Value.Set (SET [[I 0]; [I 1]; [I 2]]))
            // | 0..3 > -> |0, 1, 2>
            (ast.Ket [ast.Range(ast.Int(0), ast.Int(3))], Value.Ket (SET [[I 0]; [I 1]; [I 2]]))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            // t1 .. 10: Invalid start type
            (ast.Range (ast.Id("t1"), ast.Int(0)), "Invalid value for a range start..end: (False, 5)..0")
            // 10 .. t1: Invalid start type
            (ast.Range (ast.Int(10), ast.Id("t1")), "Invalid value for a range start..end: 10..(False, 5)")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))


    [<TestMethod>]
    member this.EvalArithmeticExpressions() =
        let ctx = this.Context

        [
            // 0 + 1 -> 1
            (ast.Add [ast.Int 0; ast.Int 1], Value.Int 1)
            // 0 + (1) + 2 + 4-> 7
            (ast.Add [ast.Int 0; ast.Tuple[ast.Int 1]; ast.Int 2; ast.Int 4], Value.Int 7)
            
            // Boolean is + mod 2
            // f + t -> t
            (ast.Add [ast.Bool false; ast.Bool true], Value.Bool true)
            // t + f -> t
            (ast.Add [ast.Bool true; ast.Bool false], Value.Bool true)
            // f + f -> f
            (ast.Add [ast.Bool false; ast.Bool false], Value.Bool false)
            // t + t -> f

            // Adding/multiplying tuples, is item-wise:
            (ast.Add [ast.Bool true; ast.Bool true], Value.Bool false)
            // (0, 3, f) + ((1, 2, t) * (3, 3, t))-> (3, 9, t)
            (ast.Add [
                ast.Tuple [ast.Int 0; ast.Int 3; ast.Bool false]
                ast.Multiply [
                    ast.Tuple [ast.Int 1; ast.Int 2; ast.Bool true]
                    ast.Tuple [ast.Int 3; ast.Int 3; ast.Bool true]]
            ], Value.Tuple [I 3; I 9; B true])

            // Adding tuples, is item-wise:
            // (0, 3, f) + (1, 2, t) (10, 20, f) -> (11, 25, t)
            (ast.Add [
                ast.Tuple [ast.Int 0; ast.Int 3; ast.Bool false]
                ast.Tuple [ast.Int 1; ast.Int 2; ast.Bool true]
                ast.Tuple [ast.Int 10; ast.Int 20; ast.Bool false]
            ], Value.Tuple [I 11; I 25; B true])
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            // 0 + 
            (ast.Add [ast.Int 0], "Need at least two operands, received: 1")
            // *
            (ast.Multiply [], "Need at least two operands, received: 0")
            // 0 + false
            (ast.Add [ast.Int 0; ast.Bool false], "Invalid operands: 0 and False")
            // true + 1 
            (ast.Add [ast.Bool true; ast.Int 1], "Invalid operands: True and 1")
            // (true, 1) + (true, true)
            (ast.Add [
                ast.Tuple [ast.Bool true; ast.Int 1]
                ast.Tuple [ast.Bool true; ast.Bool false]
            ], "Cannot evaluate: tuple elements must have be of the same type: 1 != False")
            // (true, 1) + (2, 4)
            (ast.Add [
                ast.Tuple [ast.Bool true; ast.Int 1]
                ast.Tuple [ast.Int 2; ast.Int 4]
            ], "Cannot evaluate: tuple elements must have be of the same type: True != 2")
            // (1, 1, 3) + (2, 4)
            (ast.Add [
                ast.Tuple [ast.Int 1; ast.Int 1; ast.Int 3]
                ast.Tuple [ast.Int 2; ast.Int 4]
            ], "Cannot evaluate: tuples must have the same size")
            // [1, 2] + (1, 2)
            (ast.Add [
                ast.Set [ast.Int 1; ast.Int 2]
                ast.Tuple [ast.Int 1; ast.Int 2]
            ], "Invalid operands: [1, 2] and (1, 2)")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))



    [<TestMethod>]
    member this.BoolExpressions() =
        let ctx = this.Context

        [
            // true -> true
            (ast.Bool(true), Value.Bool(true))
            // ! true -> false
            (ast.Not(ast.Bool(true)), Value.Bool(false))
            
            // true and true and true -> true
            (ast.And([ast.Bool true; ast.Bool true; ast.Bool true]), Value.Bool(true))
            // true and true and false -> false
            (ast.And([ast.Bool true; ast.Bool true; ast.Bool false]), Value.Bool(false))
            // false and true and true -> false
            (ast.And([ast.Bool false; ast.Bool true; ast.Bool true]), Value.Bool(false))
            // true and false and true -> false
            (ast.And([ast.Bool true; ast.Bool false; ast.Bool true]), Value.Bool(false))

            // true or true or true -> true
            (ast.Or([ast.Bool true; ast.Bool true; ast.Bool true]), Value.Bool(true))
            // true or false or false -> true
            (ast.Or([ast.Bool true; ast.Bool false; ast.Bool false]), Value.Bool(true))
            // false or false or true -> true
            (ast.Or([ast.Bool false; ast.Bool false; ast.Bool true]), Value.Bool(true))
            // false or false or false -> false
            (ast.Or([ast.Bool false; ast.Bool false; ast.Bool false]), Value.Bool(false))

            // 7 < 10 -> true
            (ast.LessThan(ast.Int 7, ast.Int 10), Value.Bool(true))
            // 7 < 7 -> false
            (ast.LessThan(ast.Int 7, ast.Int 7), Value.Bool(false))
            // 7 < 5 -> false
            (ast.LessThan(ast.Int 7, ast.Int 5), Value.Bool(false))

            (ast.Equals(
                ast.Set [
                    ast.Tuple [ast.Bool false; ast.Int 3; ast.Int 5]
                    ast.Tuple [ast.Bool true; ast.Int 13; ast.Int 15]
                ],
                ast.Set [
                    ast.Tuple [ast.Bool true; ast.Int 13; ast.Int 15]
                    ast.Tuple [ast.Bool false; ast.Int 3; ast.Int 5]
                ]
            ), Value.Bool(true))

            // true and false and 4
            (ast.And [
                ast.Bool true
                ast.Bool false
                ast.Int 4
            ], Value.Bool false)

            // i1 == 3 and ! false and (7) < 10 and (false, 5) == t1 and not ((1,2,3) == t1) -> true
            (ast.And [
                ast.Equals (ast.Id("i1"), ast.Int(3))
                ast.Not (ast.Bool(false))
                ast.LessThan(ast.Int 7, ast.Int 10)
                ast.Equals(ast.Tuple [ast.Bool false; ast.Int 5], ast.Id("t1"))
                ast.Not(ast.Equals(ast.Tuple [ast.Int 1; ast.Int 2; ast.Int 3], ast.Id("t1")))
            ], Value.Bool(true))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            (ast.Equals(
                ast.Set [ast.Bool false; ast.Int 3],
                ast.Ket [ast.Bool false; ast.Int 3]
            ), "Invalid expression: [False, 3] == | False, 3 >")
            (ast.Or [
                ast.Ket [ast.Bool false; ast.Int 3]
                ast.Ket [ast.Bool false; ast.Int 3]
            ], "Invalid expression: | False, 3 > or | False, 3 >")
            (ast.Not (ast.Int 4), "Invalid expression: !4")
            (ast.And [
                ast.Bool true
                ast.Bool true
                ast.Int 4
            ], "Invalid expression: True and 4")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))


    [<TestMethod>]
    member this.ProjectExpressions() =
        let ctx = this.Context

        [
            // (3,4,5).0 -> 3
            (ast.Project(ast.Tuple [ast.Int(3); ast.Int(4); ast.Int 5], [ast.Int 0]), Value.Int 3)
            // (3,4,5).1 -> 4
            (ast.Project(ast.Tuple [ast.Int(3); ast.Int(4); ast.Int 5], [ast.Int 1]), Value.Int 4)
            // (3,4,5).2 -> 5
            (ast.Project(ast.Tuple [ast.Int(3); ast.Int(4); ast.Int 5], [ast.Int 1]), Value.Int 4)
            // (3,4,5).[0,1] -> (3,4)
            (ast.Project(ast.Tuple [ast.Int(3); ast.Int(4); ast.Int 5], [ast.Int 0;ast.Int 1]), Value.Tuple[I 3; I 4])
            // (3,4,5).[0,1,2] -> (3,4,5)
            (ast.Project(ast.Tuple [ast.Int(3); ast.Int(4); ast.Int 5], [ast.Int 0;ast.Int 1; ast.Int 2]), Value.Tuple[I 3; I 4; I 5])
            // (3,4,5).[1,1] -> (4,4)
            (ast.Project(ast.Tuple [ast.Int(3); ast.Int(4); ast.Int 5], [ast.Int 1;ast.Int 1]), Value.Tuple[I 4; I 4])
            // (2,1).[0,0,0] -> (2,2,2)
            (ast.Project(ast.Tuple [ast.Int 2;ast.Int 1], [ast.Int 0; ast.Int 0; ast.Int 0]), Value.Tuple[I 2;I 2; I 2])

            // [(3,4,5)].0 -> [3]
            (ast.Project(ast.Set [ast.Tuple [ast.Int(3); ast.Int(4); ast.Int 5]], [ast.Int 0]),Value.Set (SET [[I 3]]))
            // [3,4,5].0 -> [3,4,5]
            (ast.Project(ast.Set [ast.Int(3); ast.Int(4); ast.Int 5], [ast.Int 0]),Value.Set (SET [[I 3];[I 4];[I 5]]))
            // [(1,2), (3,4), (5,6)).1 -> [2,4,6]
            (ast.Project(ast.Set [
                ast.Tuple [ast.Int 1; ast.Int 2]
                ast.Tuple [ast.Int(3); ast.Int(4)]
                ast.Tuple [ast.Int 5; ast.Int 6]
            ], [ast.Int 1]), Value.Set (SET [[I 2];[I 4];[I 6]]))
            // [(1,2,10), (3,4,11), (5,6,12)).[0,2] -> [(1,10),(3,11),(5,12)]
            (ast.Project(ast.Set [
                ast.Tuple [ast.Int 1; ast.Int 2; ast.Int 10]
                ast.Tuple [ast.Int(3); ast.Int(4); ast.Int 11]
                ast.Tuple [ast.Int 5; ast.Int 6; ast.Int 12]
            ], [ast.Int 0; ast.Int 2]), Value.Set (SET [[I 1;I 10];[I 3;I 11];[I 5;I 12]]))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            // 1.0
            (ast.Project(ast.Int 1, [ast.Int 0]), "Unable to project from Int 1")
            // (2,1).3
            (ast.Project(ast.Tuple [ast.Int 2;ast.Int 1], [ast.Int 3]), "Index in project outside of range")
            // (2,1).[0,3]
            (ast.Project(ast.Tuple [ast.Int 2;ast.Int 1], [ast.Int 0; ast.Int 3]), "Index in project outside of range")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))



    [<TestMethod>]
    member this.CallClassicExpressions() =
        let ctx = 
            this.Context
                .Add("void", Value.Classic ([], ast.Skip))
                .Add("colors", Value.Classic ([], ast.Return (ast.Tuple[ast.Int 1;ast.Int 2; ast.Int 3])))
                .Add("toTuple", Value.Classic (["a"; "b"; "c"], ast.Return (ast.Tuple[ast.Id "a";ast.Id "b"; ast.Id "c"])))
                .Add("shadow", Value.Classic (["i1"], ast.Return (ast.Id "i1")))

        [
            // void() -> ()
            (ast.CallClassic("void", []), Value.Tuple [])
            // colors() -> (1, 2, 3)
            (ast.CallClassic("colors", []), Value.Tuple [I 1; I 2; I 3])
            // shadow(21) -> 21
            (ast.CallClassic("shadow", [ast.Int 21]), Value.Int 21)
            // toTuple(10, i1 * 10, 20 == 25) -> (10, 20, true)
            (ast.CallClassic("toTuple", [
                ast.Int 10
                ast.Multiply[ast.Id "i1"; ast.Int 10]
                ast.Equals (ast.Int 20, ast.Int 25)
            ]), Value.Tuple [I 10; I 30; B false])

            // given i1 = 3
            // given classic plusone (i1) -> i1 + 1
            // plusone(i1 + 7) -> 11
            (ast.CallClassic("plusone", [ast.Add [ast.Id "i1"; ast.Int 7]]), Value.Int 11)
            // plusone (10) -> 11
            (ast.CallClassic("plusone", [ast. Int 10]), Value.Int 11)
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            (ast.CallClassic("plusone", []), "Invalid arguments: expects 1, got 0")
            (ast.CallClassic("plusone", [ast.Int 1; ast.Int 1]), "Invalid arguments: expects 1, got 2")
            (ast.CallClassic("plusone", [ast.Id "k1"]), "Invalid operands: | (0, 1, 3, False, 5), (10, 11, 13, True, 25) > and 1")
            (ast.CallClassic("plusone", [ast.Tuple [ast.Int 3; ast. Int 10]]), "Invalid operands: (3, 10) and 1")
            (ast.CallClassic("foo", [ast.Id "t1"]), "Undefined classic: foo")
            (ast.CallClassic("t1", [ast.Id "i1"]), "Undefined classic: t1")

        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))


    [<TestMethod>]
    member this.CallQuantumExpressions() =
        let ctx = 
            this.Context
                .Add("void", Value.Quantum ([], "k", ast.Skip))
                .Add("colors", Value.Quantum ([], "k", ast.Return (ast.Tuple[ast.Int 1;ast.Int 2; ast.Int 3])))
                .Add("append", Value.Quantum (["a"], "k", ast.Return (ast.Id "a")))
                .Add("shadow", Value.Quantum (["i1"], "k1", ast.Return (ast.Add [ast.Id "k1"; ast.Id "i1"])))
                .Add("qone", Value.Quantum ([], "k1", ast.Return (ast.CallClassic ("plusone", [ast.Project (ast.Id "k1", [ast.Int 0])]))))
                //TODO: .Add("last", Value.Quantum ([], "k1", ast.Return (ast.Project (ast.Id "k1", [ast.Int -1]))))

        let empty = ast.Ket []
        let k1 = ast.Ket [ast.Int 0]
        let k3 = ast.Ket [ast.Int 0; ast.Int 1; ast.Int 2]
        let tuples = ast.Ket [
            ast.Tuple [ast.Int 0; ast.Bool true]
            ast.Tuple [ast.Int 1; ast.Bool false]
            ast.Tuple [ast.Int 2; ast.Bool true]
        ]
        let triples = ast.Ket [
            ast.Tuple [ast.Int 0; ast.Int 0; ast.Bool true]
            ast.Tuple [ast.Int 1; ast.Int 1; ast.Bool false]
            ast.Tuple [ast.Int 2; ast.Int 2; ast.Bool true]
        ]

        [
            // void() |> -> ()
            (ast.CallQuantum("void", [], empty), Value.Ket (SET []))
            // void() k1 -> |0>
            (ast.CallQuantum("void", [], k1), Value.Ket (SET [[I 0]]))
            
            // colors() empty -> |>
            (ast.CallQuantum("colors", [], empty), Value.Ket (SET []))
            // colors() k1 -> |(0, 1, 2, 3)>
            (ast.CallQuantum("colors", [], k1), Value.Ket (SET [[I 0; I 1; I 2; I 3]]))
            // colors() k3 -> |(0,1, 2, 3), (1, 1, 2, 3), (2, 1, 2, 3)>
            (ast.CallQuantum("colors", [], k3), Value.Ket (SET [
                [I 0; I 1; I 2; I 3]
                [I 1; I 1; I 2; I 3]
                [I 2; I 1; I 2; I 3]
            ]))
            (ast.CallQuantum("colors", [], tuples), Value.Ket (SET [
                [I 0; B true;  I 1; I 2; I 3]
                [I 1; B false; I 1; I 2; I 3]
                [I 2; B true;  I 1; I 2; I 3]
            ]))

            
            // shadow(10) k3 -> | (0,10), (1,11), (2,12) >
            (ast.CallQuantum("shadow", [ast.Int 10], k3), Value.Ket (SET [[I 0; I 10]; [I 1; I 11]; [I 2; I 12]]))

            // append(true) empty -> |>
            (ast.CallQuantum("append", [ast.Bool true], empty), Value.Ket (SET []))
            // append( (false, false) ) k1 -> |(0, false, false)>
            (ast.CallQuantum("append", [ast.Tuple [ast.Bool false; ast.Bool false]], k1), Value.Ket (SET [
                [I 0; B false; B false]
            ]))
            // append( (false, false) ) k3 -> |(0, false, false), (1, false, false), (2, false, false)>
            (ast.CallQuantum("append", [ast.Tuple [ast.Bool false; ast.Bool false]], k3), Value.Ket (SET [
                [I 0; B false; B false]
                [I 1; B false; B false]
                [I 2; B false; B false]
            ]))
            // append( [false, false, true] ) tuples -> |(0,true,false), (0,true,true), (1,false,false), (1,false, true), (2,true,false) (2,true,true)>
            (ast.CallQuantum("append", [ast.Set [ast.Bool false; ast.Bool false; ast.Bool true]], tuples), Value.Ket (SET [
                [I 0; B true; B false]
                [I 0; B true; B true]
                [I 1; B false; B false]
                [I 1; B false; B true]
                [I 2; B true; B false]
                [I 2; B true; B true]
            ]))

            // qone() k3 -> | (0,1), (1,2), (2,3) >
            (ast.CallQuantum("qone", [], k3), Value.Ket (SET [[I 0; I 1]; [I 1; I 2]; [I 2; I 3]]))

            // qone() tuples -> | (0,true,1), (1,false,2), (2,true,3) >
            (ast.CallQuantum("qone", [], k3), Value.Ket (SET [[I 0; I 1]; [I 1; I 2]; [I 2; I 3]]))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            (ast.CallQuantum("append", [], empty), "Invalid arguments: expects 1, got 0")
            (ast.CallQuantum("append", [ast.Int 1; ast.Int 1], empty), "Invalid arguments: expects 1, got 2")
            (ast.CallQuantum("foo", [ast.Id "t1"], empty), "Undefined quantum: foo")
            (ast.CallQuantum("t1", [ast.Id "i1"], empty), "Undefined quantum: t1")
            (ast.CallQuantum("append", [ast.Id "i1"], ast.Int 777), "Expecting ket value, got: 777")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))


    [<TestMethod>]
    member this.SolveExpressions() =
        let ctx = this.Context

        [
            // Solve |(3, true), (4, false), (5, false))> -> |3>
            (ast.Solve(ast.Ket [
                ast.Tuple [ast.Int 3; ast.Bool true]
                ast.Tuple [ast.Int 4; ast.Bool false]
                ast.Tuple [ast.Int 5; ast.Bool false]
            ]), Value.Ket (SET [ [I 3] ]))
            // Solve |(3, true), (4, true), (5, true))> -> |3, 4, 5>
            (ast.Solve(ast.Ket [
                ast.Tuple [ast.Int 3; ast.Bool true]
                ast.Tuple [ast.Int 4; ast.Bool true]
                ast.Tuple [ast.Int 5; ast.Bool true]
            ]), Value.Ket (SET [ [I 3]; [I 4]; [I 5] ]))
            // Solve |(1, 1, true), (2, 4, false), (3, 5, true))> -> |(1, 1), (1, 5)>
            (ast.Solve (ast.Ket [
                ast.Tuple [ast.Int 1; ast.Int 1; ast.Bool true]
                ast.Tuple [ast.Int 2; ast.Int 4; ast.Bool false]
                ast.Tuple [ast.Int 3; ast.Int 5; ast.Bool true]
            ]), Value.Ket (SET [ [I 1; I 1]; [I 3; I 5] ]))
            // Solve |(1, 1, 13, 0) (2, 4, 24, -1), (3, 5, 35, 1))> -> |(2, 4, 24)>
            (ast.Solve (ast.Ket [
                ast.Tuple [ast.Int 1; ast.Int 1; ast.Int 13; ast.Int 0]
                ast.Tuple [ast.Int 2; ast.Int 4; ast.Int 24; ast.Int -1]
                ast.Tuple [ast.Int 3; ast.Int 5; ast.Int 35; ast.Int 10]
                ast.Tuple [ast.Int 3; ast.Int 5; ast.Int 35; ast.Int 12]
                ast.Tuple [ast.Int 2; ast.Int 4; ast.Int 24; ast.Int -1]
                ast.Tuple [ast.Int 3; ast.Int 5; ast.Int 12; ast.Int 0]
                ast.Tuple [ast.Int 3; ast.Int 5; ast.Int 25; ast.Int -1]
            ]), Value.Ket (SET [ [I 2; I 4; I 24]; [I 3; I 5; I 25] ]))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            // Solve 1
            (ast.Solve(ast.Int 1), "Solve not available for 1")
            // Solve |-1,4,5>
            (ast.Solve(ast.Ket [ast.Int -1; ast.Int 4; ast.Int 5]), "Solve expects kets of size > 2. Ket size: 1")

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
            (ast.Return(ast.Int(7)), Value.Int 7)
            (ast.Return(ast.Tuple([ast.Int(3); ast.Int(5)])), Value.Tuple([I(3); I(5)]))
            (ast.Block([ast.Return(ast.Id("b1"))]), Value.Tuple([B false]))
            (ast.Block([
                ast.Skip
                ast.Return(ast.Id("t1"))
            ]),  Value.Tuple([B(false); I(5)]))
            (ast.Block([
                ast.Skip
                ast.Return(ast.Id("i1"))
                ast.Return(ast.Id("b1"))
            ]),  Value.Tuple([I 3]))
        ]
        |> List.iter testOne

        // Make sure errors are correctly reported:
        let invalid = ast.Return(ast.Id("foo"))
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
            (ast.Let("a", ast.Int(7)), ("a", Value.Int 7))
            (ast.Let("b", ast.Tuple([ast.Int(3); ast.Int(5)])), ("b", Value.Tuple([I(3); I(5)])))
            (ast.Block([ast.Let("c", ast.Id("b1"))]), ("c", Value.Tuple([B false])))
            (ast.Block([
                ast.Skip
                ast.Let("d", ast.Id("t1"))
            ]), ("d", Value.Tuple([B(false); I(5)])))
            (ast.Block([
                ast.Skip
                ast.Let("e", ast.Id("b1"))
                ast.Let("e", ast.Id("i1"))
            ]),  ("e", Value.Tuple([I 3])))
            (ast.Block([
                ast.Skip
                ast.Let("f", ast.Id("t1"))
                ast.Skip
            ]), ("f", Value.Tuple([B(false); I(5)])))
            (ast.Block([
                ast.Skip
                ast.Let("g1", ast.Id("b1"))
                ast.Let("g2", ast.Int(23))
                ast.Let("g3", ast.Id("i1"))
            ]),  ("g2", Value.Int 23))
        ]
        |> List.iter testOne

        // Make sure errors are correctly reported:
        let invalid = ast.Let("foo", ast.Id("foo"))
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
            (ast.If(ast.Bool true,
                    ast.Return (ast.Int -1),
                    ast.Return (ast.Int 1)), 
                Value.Int -1)

            (ast.If(ast.Bool false,
                    ast.Return (ast.Int -1),
                    ast.Return (ast.Int 1)), 
                Value.Int 1)
        ]
        |> List.iter testOne

        // Make sure errors are correctly reported:
        let invalid = ast.If (ast.Int 4, ast.Return (ast.Int -1),ast.Return (ast.Int 1))
        match run (invalid, Map.empty) with
        | Fail (msg, _) -> Assert.AreEqual("Invalid condition: Int 4", msg)
        | Continue _ -> Assert.AreEqual("", "Statement returned void.")
        | Result (actual, _) -> Assert.AreEqual("", $"Statement returned {actual}. Expecting error.")


    [<TestMethod>]
    member this.CallClassicStmt() =
        let ctx = this.Context

        let program = ast.Block [
            ast.DefClassic ("echo", ["v1"], (ast.Return (ast.Id "v1")))
            ast.Return (ast.CallClassic ("echo", [ast.Int 5]))
        ]

        match run (program,Map.empty) with
        | Continue _ -> Assert.AreEqual("", "Statement returned void.")
        | Fail (msg, _) -> Assert.AreEqual("", $"Error on stmt 'echo': {msg}")
        | Result (actual, _) -> Assert.AreEqual(Value.Int 5, actual)
            

    [<TestMethod>]
    member this.CallQuantumStmt() =
        let ctx = this.Context

        let program = ast.Block [
            // let k1 = |1, 5, 10>
            ast.Let("k1", ast.Ket [ast.Int 1; ast.Int 5; ast.Int 10])
            // classic echo (v1) -> v1
            ast.DefClassic ("echo", ["v1"], (ast.Return (ast.Id "v1")))
            // quantum qecho (v3) k -> (v3, k)
            ast.DefQuantum ("qecho", ["v3"], "k", (ast.Return (ast.Tuple [ast.Id "v3"; ast.Id "k"])))
            // qecho (false) k1 -> | (false, 1), (false, 5), (false, 10) >
            ast.Return (ast.CallQuantum ("qecho", [ast.Bool false], (ast.Id "k1")))
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
            
