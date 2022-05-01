namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.compiler
open aleph.runtime.Core

[<TestClass>]
type TestCore () =

    member this.TestExpression (e, expected, ?ctx)=
        let ctx = defaultArg ctx Map.empty
        let eval = (eval<unit, unit> (fun x -> "No extension implemented" |> Error))
        
        match eval (e, ctx) with
        | Ok (actual, _) -> 
            Assert.AreEqual(expected.ToString(), actual.ToString())
        | Error msg -> 
            Assert.AreEqual(expected, msg)
            Assert.Fail()
        
    member this.TestInvalidExpression (e, expected, ?ctx)=
        let ctx = defaultArg ctx Map.empty
        let eval = (eval<unit, unit> (fun x -> "No extension implemented" |> Error))
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
        let plusone = Value.Method (["a"], ast.Add([ast.Id "a"; ast.Int 1]))

        Map[ 
            ("i1", i1)
            ("b1", b1)
            ("t1", t1)
            ("t2", t2)
            ("s1", s1)
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
    member this.CallMethodExpressions() =
        let ctx = 
            this.Context
                .Add("void", Value.Method ([], ast.Tuple []))
                .Add("colors", Value.Method ([], ast.Tuple [ast.Int 1;ast.Int 2; ast.Int 3]))
                .Add("toTuple", Value.Method (["a"; "b"; "c"], ast.Tuple [ast.Id "a";ast.Id "b"; ast.Id "c"]))
                .Add("shadow", Value.Method (["i1"], ast.Id "i1"))
                .Add("shadow", Value.Method (["i1"], ast.Block ([ast.Let ("i1", ast.Set [ast.Int 0; ast.Id "i1"])], ast.Id "i1")))

        [
            // void() -> ()
            (ast.CallMethod("void", []), Value.Tuple [])
            // colors() -> (1, 2, 3)
            (ast.CallMethod("colors", []), Value.Tuple [I 1; I 2; I 3])
            // shadow(21) -> [0, 21]
            (ast.CallMethod("shadow", [ast.Int 21]), Value.Set (SET [[I 0]; [I 21]]))
            // toTuple(10, i1 * 10, 20 == 25) -> (10, 20, true)
            (ast.CallMethod("toTuple", [
                ast.Int 10
                ast.Multiply[ast.Id "i1"; ast.Int 10]
                ast.Equals (ast.Int 20, ast.Int 25)
            ]), Value.Tuple [I 10; I 30; B false])

            // given i1 = 3
            // given classic plusone (i1) -> i1 + 1
            // plusone(i1 + 7) -> 11
            (ast.CallMethod("plusone", [ast.Add [ast.Id "i1"; ast.Int 7]]), Value.Int 11)
            // plusone (10) -> 11
            (ast.CallMethod("plusone", [ast. Int 10]), Value.Int 11)
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            (ast.CallMethod("plusone", []), "Invalid arguments: expects 1, got 0")
            (ast.CallMethod("plusone", [ast.Int 1; ast.Int 1]), "Invalid arguments: expects 1, got 2")
//            (ast.CallMethod("plusone", [ast.Id "k1"]), "Invalid operands: | (0, 1, 3, False, 5), (10, 11, 13, True, 25) > and 1")
            (ast.CallMethod("plusone", [ast.Tuple [ast.Int 3; ast. Int 10]]), "Invalid operands: (3, 10) and 1")
            (ast.CallMethod("foo", [ast.Id "t1"]), "Undefined classic: foo")
            (ast.CallMethod("t1", [ast.Id "i1"]), "Undefined classic: t1")

        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))


    // [<TestMethod>]
    // member this.ReturnStmt() =
    //     let ctx = this.Context

    //     let testOne (stmt, expected) =
    //         match run (stmt, ctx) with
    //         | Continue _ -> Assert.AreEqual("", "Statement returned void.")
    //         | Fail (msg, _) -> Assert.AreEqual("", $"Error on stmt '{stmt}': {msg}")
    //         | Result (actual, _) -> Assert.AreEqual(expected, actual)
            
    //     [
    //         (ast.Return(ast.Int(7)), Value.Int 7)
    //         (ast.Return(ast.Tuple([ast.Int(3); ast.Int(5)])), Value.Tuple([I(3); I(5)]))
    //         (ast.Block([ast.Return(ast.Id("b1"))]), Value.Tuple([B false]))
    //         (ast.Block([
    //             ast.Skip
    //             ast.Return(ast.Id("t1"))
    //         ]),  Value.Tuple([B(false); I(5)]))
    //         (ast.Block([
    //             ast.Skip
    //             ast.Return(ast.Id("i1"))
    //             ast.Return(ast.Id("b1"))
    //         ]),  Value.Tuple([I 3]))
    //     ]
    //     |> List.iter testOne

    //     // Make sure errors are correctly reported:
    //     let invalid = ast.Return(ast.Id("foo"))
    //     match run (invalid, Map.empty) with
    //     | Fail (msg, _) -> Assert.AreEqual("Unknown variable: foo", msg)
    //     | Continue _ -> Assert.AreEqual("", "Statement returned void.")
    //     | Result (actual, _) -> Assert.AreEqual("", $"Statement returned {actual}. Expecting error.")


    [<TestMethod>]
    member this.LetStmt() =
        let ctx = this.Context
        let eval = (eval<unit, unit> (fun x -> "No extension implemented" |> Error))

        let testValid (e, key, expected) =
            match eval (ast.Block ([e], ast.Id key), ctx) with
            | Error msg -> Assert.AreEqual("", $"Error on key '{key}': {msg}")
            | Ok (actual, _) -> Assert.AreEqual(expected.ToString(), actual.ToString())

        let testInvalid (e, (key: string), expected) =
            match eval (ast.Block ([e], ast.Id key), ctx) with
            | Error msg -> Assert.AreEqual(expected, msg)
            | Ok (_, ctx) ->
                let actual = ctx[key]
                Assert.AreEqual(expected, actual)


        [
            (ast.Let("a", ast.Int(7)), "a", Value.Int 7)
            (ast.Let("b", ast.Tuple([ast.Int(3); ast.Int(5)])), "b", Value.Tuple([I(3); I(5)]))
            (ast.Let("c", ast.Block([], ast.Id("b1"))), "c", Value.Tuple([B false]))
            (ast.Let("d", ast.Block([ast.Let ("i1", ast.Int 25)], ast.Id "i1")), "d", Value.Int 25)
            (ast.Let("d", ast.Block([ast.Let ("i1", ast.Int 25)], ast.Id "i1")), "i1", Value.Int 3)
            (ast.Let("e", 
                ast.Block ([
                    ast.Let ("a", ast.Int 0)
                    ast.Let ("a", ast.Int 1)
                    ast.Let ("a", ast.Int 2)], 
                    ast.Times (ast.Int 10, ast.Id "a"))), 
                "e", Value.Int 20)
            (ast.Let("f", 
                ast.Block ([
                    ast.Let ("a", ast.Int 0)
                    ast.Let ("b", ast.Int 1)
                    ast.Let ("c", ast.Int 2)],
                    ast.Plus (ast.Id "a", ast.Id "c"))),
                "f", Value.Int 2)
        ]
        |> List.iter testValid

        // Make sure errors are correctly reported:
        [
            ast.Let("alpha", ast.Id("invalid")), "invalid", "Unknown variable: invalid"
            ast.Let("beta", ast.Block([ast.Let ("internal", ast.Int 25)], ast.Id "internal")), "internal", "Unknown variable: internal"
        ]
        |> List.iter testInvalid


    // [<TestMethod>]
    // member this.IfStmt() =
    //     let ctx = this.Context

    //     let testOne (stmt, expected) =
    //         match run (stmt, ctx) with
    //         | Continue _ -> Assert.AreEqual("", "Statement returned void.")
    //         | Fail (msg, _) -> Assert.AreEqual("", $"Error on stmt '{stmt}': {msg}")
    //         | Result (actual, _) -> Assert.AreEqual(expected, actual)
            
    //     [
    //         (ast.If(ast.Bool true,
    //                 ast.Return (ast.Int -1),
    //                 ast.Return (ast.Int 1)), 
    //             Value.Int -1)

    //         (ast.If(ast.Bool false,
    //                 ast.Return (ast.Int -1),
    //                 ast.Return (ast.Int 1)), 
    //             Value.Int 1)
    //     ]
    //     |> List.iter testOne

    //     // Make sure errors are correctly reported:
    //     let invalid = ast.If (ast.Int 4, ast.Return (ast.Int -1),ast.Return (ast.Int 1))
    //     match run (invalid, Map.empty) with
    //     | Fail (msg, _) -> Assert.AreEqual("Invalid condition: Int 4", msg)
    //     | Continue _ -> Assert.AreEqual("", "Statement returned void.")
    //     | Result (actual, _) -> Assert.AreEqual("", $"Statement returned {actual}. Expecting error.")


            
