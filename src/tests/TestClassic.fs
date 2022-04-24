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
        let i1 = Value.Tuple([I 3])
        let b1 = Value.Tuple([B false])
        let t1 = Value.Tuple([B(false); I(5)])
        let t2 = Value.Tuple([B(true); I(12)])
        let s1 = Value.Set(SET([
            [B(false); I(5)]
            [B(true); I(12)]
        ]))

        Map[ 
            ("i1", i1)
            ("b1", b1)
            ("t1", t1)
            ("t2", t2)
            ("s1", s1)
        ]


    [<TestMethod>]
    member this.LiteralExpression() =
        this.TestExpression(ast.Int(5), Value.Tuple([I 5]))
        this.TestExpression(ast.Bool(true), Value.Tuple([B true]))
        this.TestExpression(ast.Bool(false), Value.Tuple([B false]))


    [<TestMethod>]
    member this.IdExpression() =
        let ctx = this.Context

        for i in ctx.Keys do
            this.TestExpression(ast.Id(i), ctx.[i], ctx)

        // Some negative cases too:
        this.TestInvalidExpression(ast.Id("i1"), Map.empty)

        Assert.IsFalse(ctx.ContainsKey "foo")
        this.TestInvalidExpression(ast.Id("foo"), ctx)


    [<TestMethod>]
    member this.TuplesExpression() =
        let ctx = this.Context

        [
            (ast.Tuple([]), Value.Tuple([]))
            (ast.Tuple([ast.Int(3)]), Value.Tuple([I(3)]))
            (ast.Tuple([ast.Int(3); ast.Int(5)]), Value.Tuple([I(3); I(5)]))
            (ast.Tuple([ast.Bool(false); ast.Id("b1"); ast.Bool(true)]), Value.Tuple([B(false); B(false); B(true)]))
            (ast.Tuple([ast.Id("i1")]), Value.Tuple([I(3)]))
            // ((3)) --> (3)
            (ast.Tuple([ast.Tuple([ast.Int(3)])]), Value.Tuple([I(3)]))
            // ((0,1)) --> (0,1)
            (ast.Tuple([ast.Tuple([ast.Int(0); ast.Int(1)])]), Value.Tuple([I(0); I(1)])) // Tuple with tuple inside            
            // ((0,1), 2, ((3, 4), 5)) --> (0,1,2,3,4,5)
            (ast.Tuple([
                    ast.Tuple([ast.Int(0); ast.Int(1)])
                    ast.Int(2)
                    ast.Tuple([
                        ast.Tuple([ast.Int(3); ast.Int(4)])
                        ast.Int(5)
                    ])
                ]), Value.Tuple([I(0); I(1); I(2); I(3); I(4); I(5)])) // Tuple with tuple inside            
        ]
        |> List.map (fun (e, v) -> this.TestExpression (e, v, ctx))
        |> ignore

        [
            ast.Tuple([ast.Id("s1")])            // Tuple with set inside
            ast.Tuple([ast.Id("foo")])           // Tuple with invalid id
        ]
        |> List.map (fun n -> this.TestInvalidExpression (n, ctx))
        |> ignore



    [<TestMethod>]
    member this.SetExpression() =
        let ctx = this.Context

        [
            // []
            (ast.Set([]), Value.Set(SET([])))
            
            // [0]
            (ast.Set([ast.Int(0)]), Value.Set(SET([
                [I(0)]
            ])))
            
            // [1, 2]
            (ast.Set([ast.Int(1); ast.Int(2)]), Value.Set(SET([
                [I(1)]
                [I(2)]
            ])))
            
            //[(1,t), (3,f)]
            (ast.Set([
                ast.Tuple[ast.Int(1); ast.Bool(true)]
                ast.Tuple[ast.Int(3); ast.Bool(false)]
            ]), Value.Set(SET([
                [I(1); B(true)]
                [I(3); B(false)]
            ])))
            
            // [ t1 ]
            (ast.Set([ast.Id("t1")]), Value.Set(SET([
                [B(false); I(5)]
            ])))

            // [ t1; t1 ] --> [ t1 ]
            (ast.Set([ast.Id("t1");ast.Id("t1")]), Value.Set(SET([
                [B(false); I(5)]
            ])))
            
            // [ t1, (true, 12) ] --> [ (false, 5), (true, 12) ]
            (ast.Set([
                ast.Id("t1")
                ast.Tuple([ast.Bool(true); ast.Int(12)])
            ]), Value.Set(SET([
                [B(false); I(5)]
                [B(true); I(12)]
            ])))
            
            // [ [ 0, 1 ] ] --> [ 0, 1 ]
            (ast.Set([
                ast.Set[ast.Int(0); ast.Int(1)]
            ]), Value.Set(SET([
                [I(0)]
                [I(1)]
            ])))


            (ast.Set([ast.Id("s1")]), ctx["s1"])            // Tuple with set inside
            // (ast.Tuple([ast.Bool(false); ast.Id("b1"); ast.Bool(true)]), Value.Tuple([B(false); B(false); B(true)]))
            // (ast.Tuple([ast.Id("i1")]), Value.Tuple([I(3)]))
            // // ((3)) --> (3)
            // (ast.Tuple([ast.Tuple([ast.Int(3)])]), Value.Tuple([I(3)]))
            // // ((0,1)) --> (0,1)
            // (ast.Tuple([ast.Tuple([ast.Int(0); ast.Int(1)])]), Value.Tuple([I(0); I(1)])) // Tuple with tuple inside            
            // // ((0,1), 2, ((3, 4), 5)) --> (0,1,2,3,4,5)
            // (ast.Tuple([
            //         ast.Tuple([ast.Int(0); ast.Int(1)])
            //         ast.Int(2)
            //         ast.Tuple([
            //             ast.Tuple([ast.Int(3); ast.Int(4)])
            //             ast.Int(5)
            //         ])
            //     ]), Value.Tuple([I(0); I(1); I(2); I(3); I(4); I(5)])) // Tuple with tuple inside            
        ]
        |> List.map (fun (e, v) -> this.TestExpression (e, v, ctx))
        |> ignore

        [
            //[t,1]
            ast.Set([ast.Bool(true); ast.Int(1)])
            //[1, (2, 3)]
            ast.Set([
                ast.Int(1)
                ast.Tuple[ast.Int(2);ast.Int(3)]
            ])
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
