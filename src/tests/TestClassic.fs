namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.compiler
open aleph.runtime.Classic

[<TestClass>]
type TestClassic () =

    member this.testExpression (e, expected, ?ctx)=
        let ctx = defaultArg ctx Map.empty
        match (eval ctx e) with
        | Ok actual -> Assert.AreEqual(expected, actual)
        | Result.Error msg -> 
            Assert.AreEqual("", msg)
            Assert.Fail()
        
    member this.testInvalidExpression (e, ?ctx)=
        let ctx = defaultArg ctx Map.empty
        match (eval ctx e)  with
        | Result.Error(msg) -> Assert.IsTrue(true)
        | _ -> Assert.Fail()

    [<TestMethod>]
    member this.eval_literals() =
        this.testExpression(ast.Int(5), Value.Int(5))
        this.testExpression(ast.Bool(true), Value.Bool(true))
        this.testExpression(ast.Bool(false), Value.Bool(false))


    [<TestMethod>]
    member this.eval_ids() =
        let i1 = Value.Int(3)
        let b1 = Value.Bool(false)
        let t1 = Value.Tuple([B(false); I(5)])
        let t2 = Value.Tuple([B(true); I(12)])
        let s1 = Value.Set([
            [B(false); I(5)]
            [B(true); I(12)]
        ])

        let ctx = 
            Map[ 
                ("i1", i1)
                ("b1", b1)
                ("t1", t1)
                ("t2", t2)
                ("s1", s1)
            ]
        this.testExpression(ast.Id("i1"), i1, ctx)
        this.testExpression(ast.Id("b1"), b1, ctx)
        this.testExpression(ast.Id("s1"), s1, ctx)

        this.testInvalidExpression(ast.Id("i1"), Map.empty)
        this.testInvalidExpression(ast.Id("foo"), ctx)
