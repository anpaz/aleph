namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.compiler
open aleph.runtime.Classic

[<TestClass>]
type TestClassic () =

    member this.testExpression (e, expected, ?ctx)=
        let ctx = defaultArg ctx Map.empty
        match eval (e, ctx) with
        | Ok (actual, _) -> 
            Assert.AreEqual(expected, actual)
        | Result.Error msg -> 
            Assert.AreEqual("", msg)
            Assert.Fail()
        
    member this.testInvalidExpression (e, ?ctx)=
        let ctx = defaultArg ctx Map.empty
        match eval (e, ctx)  with
        | Result.Error(msg) -> 
            Assert.IsTrue(true)
        | _ ->
            Assert.AreEqual("", $"Expression passed: {e}")

    member this.context =
        let i1 = Value.Int(3)
        let b1 = Value.Bool(false)
        let t1 = Value.Tuple([B(false); I(5)])
        let t2 = Value.Tuple([B(true); I(12)])
        let s1 = Value.Set([
            [B(false); I(5)]
            [B(true); I(12)]
        ])

        Map[ 
            ("i1", i1)
            ("b1", b1)
            ("t1", t1)
            ("t2", t2)
            ("s1", s1)
        ]


    [<TestMethod>]
    member this.eval_literals() =
        this.testExpression(ast.Int(5), Value.Int(5))
        this.testExpression(ast.Bool(true), Value.Bool(true))
        this.testExpression(ast.Bool(false), Value.Bool(false))


    [<TestMethod>]
    member this.eval_ids() =
        let ctx = this.context

        for i in ctx.Keys do
            this.testExpression(ast.Id(i), ctx.[i], ctx)

        // Some negative cases too:
        this.testInvalidExpression(ast.Id("i1"), Map.empty)

        Assert.IsFalse(ctx.ContainsKey "foo")
        this.testInvalidExpression(ast.Id("foo"), ctx)

    [<TestMethod>]
    member this.eval_tuples() =
        let ctx = this.context

        [
            (ast.Tuple([]), Value.Tuple([]))
            (ast.Tuple([ast.Int(3)]), Value.Tuple([I(3)]))
            (ast.Tuple([ast.Int(3); ast.Int(5)]), Value.Tuple([I(3); I(5)]))
            (ast.Tuple([ast.Bool(false); ast.Id("b1"); ast.Bool(true)]), Value.Tuple([B(false); B(false); B(true)]))
            (ast.Tuple([ast.Id("i1")]), Value.Tuple([I(3)]))
        ]
        |> List.map (fun (e, v) -> this.testExpression (e, v, ctx))
        |> ignore

        [
            ast.Tuple([ast.Tuple([ast.Int(3)])]) // Tuple with tuple inside            
            ast.Tuple([ast.Id("s1")])            // Tuple with set inside
            ast.Tuple([ast.Id("foo")])           // Tuple with invalid id
        ]
        |> List.map (fun n -> this.testInvalidExpression (n, ctx))
        |> ignore



    [<TestMethod>]
    member this.return_value() =
        let ctx = this.context

        let testOne (stmt, expected) =
            match run (stmt, ctx) with
            | Continue _ -> Assert.AreEqual("", "Statement returned void.")
            | Error (msg, _) -> Assert.AreEqual("", msg)
            | Result (actual, _) -> Assert.AreEqual(expected, actual)
            

        [
            (ast.Return(ast.Int(7)), Value.Int(7))
            (ast.Return(ast.Tuple([ast.Int(3); ast.Int(5)])), Value.Tuple([I(3); I(5)]))
            (ast.Block([ast.Return(ast.Id("b1"))]), Value.Bool(false))
            (ast.Block([
                ast.Skip
                ast.Return(ast.Id("t1"))
            ]),  Value.Tuple([B(false); I(5)]))
            (ast.Block([
                ast.Skip
                ast.Return(ast.Id("i1"))
                ast.Return(ast.Id("b1"))
            ]),  Value.Int(3))
        ]
        |> List.map testOne
        |> ignore


        // Make sure errors are correctly reported:
        let invalid = ast.Return(ast.Id("foo"))
        match run (invalid, Map.empty) with
        | Error (msg, _) -> Assert.AreEqual("Unknown variable: foo", msg)
        | Continue _ -> Assert.AreEqual("", "Statement returned void.")
        | Result (actual, _) -> Assert.AreEqual("", $"Statement returned {actual}. Expecting error.")