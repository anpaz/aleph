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
            Assert.AreEqual("", msg)
            Assert.Fail()
        
    member this.TestInvalidExpression (e, ?ctx)=
        let ctx = defaultArg ctx Map.empty
        match eval (e, ctx)  with
        | Result.Error(_) -> 
            Assert.IsTrue(true)
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

        Map[ 
            ("i1", i1)
            ("b1", b1)
            ("t1", t1)
            ("t2", t2)
            ("s1", s1)
            ("k1", k1)
        ]


    [<TestMethod>]
    member this.LiteralExpressions() =
        this.TestExpression(ast.Int(5), Value.Tuple([I 5]))
        this.TestExpression(ast.Bool(true), Value.Tuple([B true]))
        this.TestExpression(ast.Bool(false), Value.Tuple([B false]))


    [<TestMethod>]
    member this.IdExpressions() =
        let ctx = this.Context

        for i in ctx.Keys do
            this.TestExpression(ast.Id(i), ctx.[i], ctx)

        // Some negative cases too:
        this.TestInvalidExpression(ast.Id("i1"), Map.empty)

        Assert.IsFalse(ctx.ContainsKey "foo")
        this.TestInvalidExpression(ast.Id("foo"), ctx)


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
            // (i1) -> (3)
            (ast.Tuple([ast.Id("i1")]), Value.Tuple([I(3)]))
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
        |> List.map (fun (e, v) -> this.TestExpression (e, v, ctx))
        |> ignore

        [
            ast.Tuple([ast.Id("foo")])           // Tuple with invalid id
        ]
        |> List.map (fun n -> this.TestInvalidExpression (n, ctx))
        |> ignore



    [<TestMethod>]
    member this.SetExpressions() =
        let ctx = this.Context

        // ---------------------------------- //
        // Positive cases
        // ---------------------------------- //
        [
            // []
            (ast.Set([]), Value.Set(SET([])))
            
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

            // [ [ (0,0,0), (1,1,1) ], [(0,1,0), (1,1,1)]  ] --> [ (0,0), (0,1), (1,1) ] ]
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
        |> List.map (fun (e, v) -> this.TestExpression (e, v, ctx))
        |> ignore

        // ---------------------------------- //
        // Negative cases
        // ---------------------------------- //
        [
            //[1, 2, (2,3)]
            ast.Set [
                ast.Int(1)
                ast.Int(2)
                ast.Tuple([ ast.Int(2); ast.Int(3) ])
            ]
            //[1, (2, 3)] : Uneven lenghts
            ast.Set [
                ast.Int(1)
                ast.Tuple([ast.Int(2);ast.Int(3)]) ]
            //[ [(0,0), (1,2)], [(2, 3, 4)] ] : Uneven embeded lenghts
            ast.Set [
                ast.Set [
                    ast.Tuple([ast.Int(0); ast.Int(0)])
                    ast.Tuple([ast.Int(1); ast.Int(2)])
                ]
                ast.Set[
                    ast.Tuple([ast.Int(2); ast.Int(3); ast.Int(4)])
                ]
            ]
            //[ | 1, 2> ] : Set of kets
            ast.Set [
                ast.Ket [
                    ast.Int(1)
                    ast.Int(2) ]
            ]
        ]
        |> List.map (fun n -> this.TestInvalidExpression (n, ctx))
        |> ignore


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

            // | s1 > --> 
            (ast.Ket [ ast.Id "s1"]), Value.Ket(SET[
                [B false; I 5]
                [B true; I 12]
            ])
        ]
        |> List.map (fun (e, v) -> this.TestExpression (e, v, ctx))
        |> ignore


        // ---------------------------------- //
        // Negative cases
        // ---------------------------------- //
        [
            //  given k1 = | (F, 5), (T,12) >
            // | k1 >
            ast.Ket [ast.Id("k1")]

            //| | 1, 2> >
            ast.Ket [
                ast.Ket [
                    ast.Int(1)
                    ast.Int(2) ]
            ]
        ]
        |> List.map (fun n -> this.TestInvalidExpression (n, ctx))
        |> ignore


    [<TestMethod>]
    member this.ReturnStmt() =
        let ctx = this.Context

        let testOne (stmt, expected) =
            match run (stmt, ctx) with
            | Continue _ -> Assert.AreEqual("", "Statement returned void.")
            | Error (msg, _) -> Assert.AreEqual("", $"Error on stmt '{stmt}': {msg}")
            | Result (actual, _) -> Assert.AreEqual(expected, actual)
            
        [
            (ast.Return(ast.Int(7)), Value.Tuple([I 7]))
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
        |> List.map testOne
        |> ignore

        // Make sure errors are correctly reported:
        let invalid = ast.Return(ast.Id("foo"))
        match run (invalid, Map.empty) with
        | Error (msg, _) -> Assert.AreEqual("Unknown variable: foo", msg)
        | Continue _ -> Assert.AreEqual("", "Statement returned void.")
        | Result (actual, _) -> Assert.AreEqual("", $"Statement returned {actual}. Expecting error.")


    [<TestMethod>]
    member this.LetStmt() =
        let ctx = this.Context

        let testOne (stmt, (key, expected)) =
            match run (stmt, ctx) with
            | Error (msg, _) -> Assert.AreEqual("", $"Error on key '{key}': {msg}")
            | Result (actual, _) -> Assert.AreEqual("", $"Statement return Result: {actual}.")
            | Continue ctx -> 
                let actual = ctx[key]
                Assert.AreEqual(expected, actual)

        [
            (ast.Let("a", ast.Int(7)), ("a", Value.Tuple([I 7])))
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
            ]),  ("g2", Value.Tuple([I 23])))
        ]
        |> List.map testOne
        |> ignore

        // Make sure errors are correctly reported:
        let invalid = ast.Let("foo", ast.Id("foo"))
        match run (invalid, Map.empty) with
        | Error (msg, _) -> Assert.AreEqual("Unknown variable: foo", msg)
        | Continue _ -> Assert.AreEqual("", "Statement returned void.")
        | Result (actual, _) -> Assert.AreEqual("", $"Statement returned {actual}. Expecting error.")
