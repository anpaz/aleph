namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.parser.ast
open aleph.parser.TypeChecker
open aleph.parser.ast.typed
open aleph.runtime.Eval


module ClassicValueContext =

    let NoQPU =
        { new QPU with
            member this.Measure(arg1: IUniverse, ctx: EvalContext) : Result<Value * QuantumGraph, string> = failwith "Not Implemented"

            member this.Prepare(arg1: U, arg2: EvalContext) : Result<Value * QuantumGraph, string> = failwith "Not Implemented" }

    let Prelude =
        [ s.Let("i1", e.Int 1)
          s.Let("b1", e.Bool true)
          s.Let("t1", e.Tuple [ e.Bool false; e.Int 1 ])
          s.Let("t2", e.Tuple [ e.Bool true; e.Int 2 ])
          s.Let("t3", e.Tuple [ e.Int 0; e.Int 1; e.Int 2 ])
          s.Let(
              "s1",
              e.Set
                  [ e.Tuple [ e.Bool false; e.Int 0 ]
                    e.Tuple [ e.Bool false; e.Int 1 ]
                    e.Tuple [ e.Bool false; e.Int 2 ] ]
          ) ]

[<TestClass>]
type TestEval() =

    member this.TestExpression prelude (expr, v) =
        printfn "e: %A" expr

        let block = e.Block(prelude, expr)

        match aleph.runtime.Eval.apply (block, ClassicValueContext.NoQPU) with
        | Ok (v', _) -> Assert.AreEqual(v, v')
        | Error msg -> Assert.AreEqual($"Expecting Value {v}", $"Got Error msg: {msg}")

    member this.TestInvalidExpression stmts (expr, error) =
        printfn "e: %A" expr

        let block = e.Block(stmts, expr)

        match aleph.runtime.Eval.apply (block, ClassicValueContext.NoQPU) with
        | Ok (v, _) -> Assert.AreEqual($"Expected error: {error}", $"Got Value: {v}")
        | Error msg -> Assert.AreEqual(error, msg)

    [<TestMethod>]
    member this.TestClassicLiterals() =
        let prelude = ClassicValueContext.Prelude

        let emptyCtx: EvalContext =
            { heap = Map.empty
              graph = QuantumGraph.empty
              qpu = ClassicValueContext.NoQPU }

        [
          // false
          e.Bool false, Value.Bool false
          // 5
          e.Int 5, Value.Int 5
          // (false, 0, 1)
          e.Tuple [ e.Bool false; e.Int 0; e.Int 1 ], Value.Tuple [ Bool false; Int 0; Int 1 ]
          // {}
          e.Set [], Value.Set(Set.empty)
          // {false}
          e.Set [ e.Bool false ], Value.Set(Set.ofList [ Bool false ])
          // {0, 1, 2}
          e.Set [ e.Int 0; e.Int 1; e.Int 2 ], Value.Set(Set.ofList [ Int 0; Int 1; Int 2 ])
          // {(false, 0, 0), (true, 0, 1), (true, 1, 1)}
          e.Set
              [ e.Tuple [ e.Bool false; e.Int 0; e.Int 0 ]
                e.Tuple [ e.Bool true; e.Int 0; e.Int 1 ]
                e.Tuple [ e.Bool true; e.Int 1; e.Int 1 ] ],
          Value.Set(
              Set.ofList
                  [ Tuple [ Bool false; Int 0; Int 0 ]
                    Tuple [ Bool true; Int 0; Int 1 ]
                    Tuple [ Bool true; Int 1; Int 1 ] ]
          )
          // { 1..5 }
          e.Range(e.Int 1, e.Int 5), Value.Set(Set.ofList [ Int 1; Int 2; Int 3; Int 4 ])
          // function (a: Int) -> a + 1
          e.Method(arguments = [ "a", Type.Int ], returns = Type.Int, body = (e.Add(e.Var "a", e.Int 1))),
          Value.Method
              { Args = [ "a" ]
                Body = (E.Classic(C.Add((C.Var "a", C.IntLiteral 1)), Type.Int))
                Context = emptyCtx }
          // (k1: Ket<Int>, k2: Ket<Int, Bool>) -> (k1, k2)
          e.Method(
              arguments = [ "k1", (Type.Ket [ Type.Int ]); "k2", (Type.Ket [ Type.Int; Type.Bool ]) ],
              returns = (Type.Ket [ Type.Int; Type.Int; Type.Bool ]),
              body = (e.Join(e.Var "k1", e.Var "k2"))
          ),
          Value.Method
              { Args = [ "k1"; "k2" ]
                Body = (E.Quantum(Q.Join(Q.Var "k1", Q.Var "k2"), (Type.Ket [ Type.Int; Type.Int; Type.Bool ])))
                Context = emptyCtx }
          // (k1: Ket<Int>, k2: Ket<Int, Bool>) -> Prepare(k2)
          e.Method(
              arguments = [ "k1", (Type.Ket [ Type.Int ]); "k2", (Type.Ket [ Type.Int; Type.Bool ]) ],
              returns = (Type.Universe [ Type.Int; Type.Bool ]),
              body = (e.Prepare(e.Var "k2"))
          ),
          Value.Method
              { Args = [ "k1"; "k2" ]
                Body = (E.Universe(U.Prepare(Q.Var "k2"), (Type.Universe [ Type.Int; Type.Bool ])))
                Context = emptyCtx } ]
        |> List.iter (this.TestExpression prelude)


    [<TestMethod>]
    member this.TestBinaryExpressions() =
        let prelude = ClassicValueContext.Prelude

        [
          // true || true
          e.Or(e.Bool true, e.Bool true), Value.Bool true
          // false || true
          e.Or(e.Bool false, e.Bool true), Value.Bool true
          // false && true
          e.And(e.Bool false, e.Bool true), Value.Bool false
          // false && false
          e.And(e.Bool false, e.Bool false), Value.Bool false
          // (5 + 10) * 4
          e.Multiply(e.Add(e.Int 5, e.Int 10), e.Int 4), Value.Int 60
          // (5 + 10) * 4 == 100
          e.Equals(e.Multiply(e.Add(e.Int 5, e.Int 10), e.Int 4), e.Int 100), Value.Bool false
          // (5 + 10) * 4 == 60
          e.Equals(e.Multiply(e.Add(e.Int 5, e.Int 10), e.Int 4), e.Int 60), Value.Bool true
          // (5 + 10) * 4 == 100 || true
          e.Or(e.Equals(e.Multiply(e.Add(e.Int 5, e.Int 10), e.Int 4), e.Int 100), e.Bool true), Value.Bool true
          // (5 + 10) * 4 < 50
          e.LessThan(e.Multiply(e.Add(e.Int 5, e.Int 10), e.Int 4), e.Int 50), Value.Bool false
          // (5 + 10) * 4 < 100
          e.LessThan(e.Multiply(e.Add(e.Int 5, e.Int 10), e.Int 4), e.Int 100), Value.Bool true
          // not ((5 + 10) * 4 < 50)
          e.Not(e.LessThan(e.Multiply(e.Add(e.Int 5, e.Int 10), e.Int 4), e.Int 50)), Value.Bool true ]
        |> List.iter (this.TestExpression prelude)

    [<TestMethod>]
    member this.TestIfExpressions() =
        let prelude = ClassicValueContext.Prelude

        [
          // if true then 10 else 20
          e.If(e.Bool true, e.Int 10, e.Int 20), Value.Int 10
          // if false then 10 else 20 + 20
          e.If(e.Bool false, e.Int 10, e.Add(e.Int 20, e.Int 20)), Value.Int 40 ]
        |> List.iter (this.TestExpression prelude)

        [
          // if 1 then 10 else 20
          e.If(e.Int 1, e.Int 10, e.Int 20), "If condition must be a boolean, got Int" ]
        |> List.iter (this.TestInvalidExpression prelude)


    [<TestMethod>]
    member this.TestProjectExpressions() =
        let prelude = ClassicValueContext.Prelude

        [
          // t1[0]
          e.Project(e.Var "t1", e.Int 0), Value.Bool false
          // t2[1]
          e.Project(e.Var "t2", e.Int 1), Value.Int 2
          // t3[0 + t2[1]]
          e.Project(e.Var "t3", e.Add(e.Int 0, e.Project(e.Var "t2", e.Int 1))), Value.Int 2
          // (t3, t2)[2]
          e.Project(e.Join(e.Var "t3", e.Var "t2"), e.Int 1), Value.Int 1
          // (t3, t2)[3]
          e.Project(e.Join(e.Var "t3", e.Var "t2"), e.Int 3), Value.Bool true
          // (t3, t3)[2 + 2]
          e.Project(e.Join(e.Var "t3", e.Var "t3"), e.Add(e.Int 2, e.Int 2)), Value.Int 1 ]
        |> List.iter (this.TestExpression prelude)

        [
          // t1[t2[0]]
          e.Project(e.Var "t2", e.Project(e.Var "t2", e.Int 0)),
          "Invalid projection index. Expected int expression, got: Classic (Project (Var \"t2\", 0), Bool)" ]
        |> List.iter (this.TestInvalidExpression prelude)

    [<TestMethod>]
    member this.TestCallMethodExpressions() =
        let prelude =
            ClassicValueContext.Prelude
            @ [ s.Let(
                    "m0",
                    e.Method(
                        arguments = [],
                        returns = Type.Tuple [ Type.Int; Type.Int ],
                        body = e.Tuple [ e.Int 1; e.Int 2 ]
                    )
                )
                s.Let(
                    "m1",
                    e.Method(
                        arguments = [ ("a", Type.Int); ("b", Type.Int) ],
                        returns = Type.Int,
                        body = e.Add(e.Var "a", e.Var "b")
                    )
                )
                s.Let(
                    "m2",
                    e.Method(
                        arguments =
                            [ ("x", (Type.Tuple [ Type.Bool; Type.Bool; Type.Bool ]))
                              ("y", Type.Bool)
                              ("z", Type.Int) ],
                        returns = Type.Bool,
                        body = e.Or(e.Var "y", e.Project(e.Var "x", e.Var "z"))
                    )
                ) ]

        [
          // m0 ()
          e.CallMethod(e.Var "m0", []), Value.Tuple [ Value.Int 1; Value.Int 2 ]
          // m1 (10, 20)
          e.CallMethod(e.Var "m1", [ e.Int 10; e.Int 20 ]), Value.Int 30
          // m2 ((10, false, true), false, 2)
          e.CallMethod(e.Var "m2", [ e.Tuple [ e.Bool false; e.Bool false; e.Bool true ]; e.Bool false; e.Int 2 ]),
          Value.Bool true
          // m2 ((10, false, true), false, 1)
          e.CallMethod(e.Var "m2", [ e.Tuple [ e.Bool true; e.Bool false; e.Bool true ]; e.Bool false; e.Int 1 ]),
          Value.Bool false ]
        |> List.iter (this.TestExpression prelude)


    [<TestMethod>]
    member this.TestSetExpressions() =
        let prelude = ClassicValueContext.Prelude

        [
          // (Append 3, {})
          e.Append(e.Int 3, e.Set []), Value.Set(Set.ofList [ Int 3 ])
          // (Append false, {false})
          e.Append(e.Bool false, e.Set [ e.Bool false ]), Value.Set(Set.ofList [ Bool false ])
          // (Append 1, {0, 4, 2})
          e.Append(e.Int 1, e.Set [ e.Int 0; e.Int 4; e.Int 2 ]), Value.Set(Set.ofList [ Int 0; Int 1; Int 2; Int 4 ])

          // (Remove false, {false})
          e.Remove(e.Bool false, e.Set [ e.Bool false ]), Value.Set(Set.ofList [])
          // (Remove 4, {0, 4, 2})
          e.Remove(e.Int 4, e.Set [ e.Int 0; e.Int 4; e.Int 2 ]), Value.Set(Set.ofList [ Int 0; Int 2 ])
          // (Remove (1,2), {(0,0), (1,1), (1,2)})
          e.Remove(
              e.Tuple [ e.Int 1; e.Int 2 ],
              e.Set
                  [ e.Tuple [ e.Int 0; e.Int 0 ]
                    e.Tuple [ e.Int 1; e.Int 1 ]
                    e.Tuple [ e.Int 1; e.Int 2 ] ]
          ),
          Value.Set(Set.ofList [ Tuple [ Int 0; Int 0 ]; Tuple [ Int 1; Int 1 ] ])

          // (Count {})
          e.Count(e.Set []), Int 0
          // (Count {false})
          e.Count(e.Set [ e.Bool false ]), Int 1
          // ({false, false, true})
          e.Count(e.Set [ e.Bool false; e.Bool false; e.Bool true ]), Int 2
          // (Count {(0,0), (1,1), (1,2)})
          e.Count(
              e.Set
                  [ e.Tuple [ e.Int 0; e.Int 0 ]
                    e.Tuple [ e.Int 1; e.Int 1 ]
                    e.Tuple [ e.Int 1; e.Int 2 ] ]
          ),
          Int 3

          // (Remove(Element s1), s1), Count(s1))
          e.Tuple [ e.Count(e.Remove(e.Element(e.Var "s1"), e.Var "s1")); e.Count(e.Var "s1") ], Tuple [ Int 2; Int 3 ]
          // (Count {(0,0), (1,1), (1,2)})
          e.Element(
              e.Remove(
                  e.Tuple [ e.Int 1; e.Int 1 ],
                  e.Remove(
                      e.Tuple [ e.Int 0; e.Int 0 ],
                      e.Set
                          [ e.Tuple [ e.Int 0; e.Int 0 ]
                            e.Tuple [ e.Int 1; e.Int 1 ]
                            e.Tuple [ e.Int 1; e.Int 2 ] ]
                  )
              )
          ),
          Tuple [ Int 1; Int 2 ]

          ]
        |> List.iter (this.TestExpression prelude)


    [<TestMethod>]
    member this.TestVariableScope() =
        let prelude = ClassicValueContext.Prelude

        [
          // let x = (1, 2)
          // x
          e.Block([ s.Let("x", e.Tuple [ e.Int 1; e.Int 2 ]) ], (e.Var "x")), Value.Tuple [ Value.Int 1; Value.Int 2 ]

          // let alpha = 1
          // let x() = alpha
          // x()
          e.Block(
              [ s.Let("alpha", e.Int 1); s.Let("x", e.Method([], (Type.Int), e.Var "alpha")) ],
              e.CallMethod(e.Var "x", [])
          ),
          Value.Int 1

          // let x = 10
          // let y = x
          // let x = false
          // (x, y)
          e.Block(
              [ s.Let("x", e.Int 10); s.Let("y", e.Var "x"); s.Let("x", e.Bool false) ],
              e.Tuple [ e.Var "x"; e.Var "y" ]
          ),
          Value.Tuple [ Value.Bool false; Value.Int 10 ]

          // let x =
          //    if true then
          //      let y = 10
          //      y
          //    else
          //      let z = 20
          //      z
          // x
          e.Block(
              [ s.Let(
                    "x",
                    e.If(
                        e.Bool true,
                        e.Block([ s.Let("y", e.Int 10) ], e.Var "y"),
                        e.Block([ s.Let("z", e.Int 20) ], e.Var "z")
                    )
                ) ],
              e.Var "x"
          ),
          Value.Int 10

          // let foo = 100
          // let x =
          //    if false then
          //      let y = 10
          //      y
          //    else
          //      foo
          // x
          e.Block(
              [ s.Let("foo", e.Int 100)
                s.Let("x", e.If(e.Bool false, e.Block([ s.Let("y", e.Int 10) ], e.Var "y"), e.Var "foo")) ],
              e.Var "x"
          ),
          Value.Int 100

          // let alpha = 1
          // let x =
          //    if true then
          //      let alpha = 10
          //      alpha
          //    else
          //      let alpha = 20
          //      alpha
          // (alpha, x)
          e.Block(
              [ s.Let("alpha", e.Int 1)
                s.Let(
                    "x",
                    e.If(
                        e.Bool true,
                        e.Block([ s.Let("alpha", e.Int 10) ], e.Var "alpha"),
                        e.Block([ s.Let("alpha", e.Int 20) ], e.Var "alpha")
                    )
                ) ],
              e.Tuple [ e.Var "alpha"; e.Var "x" ]
          ),
          Value.Tuple [ Value.Int 1; Value.Int 10 ]

          // let alpha = 1
          // let x =
          //    if false then
          //      let alpha = 10
          //      alpha
          //    else
          //      let alpha = 20
          //      alpha
          // (alpha, x)
          e.Block(
              [ s.Let("alpha", e.Int 1)
                s.Let(
                    "x",
                    e.If(
                        e.Bool false,
                        e.Block([ s.Let("alpha", e.Int 10) ], e.Var "alpha"),
                        e.Block([ s.Let("alpha", e.Int 20) ], e.Var "alpha")
                    )
                ) ],
              e.Tuple [ e.Var "alpha"; e.Var "x" ]
          ),
          Value.Tuple [ Value.Int 1; Value.Int 20 ]


          // let alpha = 1
          // let beta = 10
          // let x =
          //    let alpha = alpha * beta
          //    let beta = 20
          //    let x =
          //      let alpha = alpha * beta
          //      alpha
          //    (alpha, beta, x)
          // (Join (alpha, beta), x)
          e.Block(
              [ s.Let("alpha", e.Int 1)
                s.Let("beta", e.Int 10)
                s.Let(
                    "x",
                    e.Block(
                        [ s.Let("alpha", e.Multiply(e.Var "alpha", e.Var "beta"))
                          s.Let("beta", e.Int 20)
                          s.Let(
                              "x",
                              e.Block([ s.Let("alpha", e.Multiply(e.Var "alpha", e.Var "beta")) ], e.Var "alpha")
                          ) ],
                        e.Tuple [ e.Var "alpha"; e.Var "beta"; e.Var "x" ]
                    )
                ) ],
              e.Join(e.Tuple [ e.Var "alpha"; e.Var "beta" ], e.Var "x")
          ),
          Value.Tuple [ Value.Int 1; Value.Int 10; Value.Int 10; Value.Int 20; Value.Int 200 ]

          // let a = 3
          // let foo(b) = a + b
          // let a = 25
          // foo(4)
          e.Block(
              [ s.Let("a", e.Int 3)
                s.Let(
                    "foo",
                    e.Method(arguments = [ "b", Type.Int ], returns = (Type.Int), body = e.Add(e.Var "a", e.Var "b"))
                ) ],
              e.CallMethod(e.Var "foo", [ e.Int 4 ])
          ),
          Value.Int 7 ]
        |> List.iter (this.TestExpression prelude)


        [
          // let x =
          //    if true then
          //      let y = 10
          //      y
          //    else
          //      let z = 20
          //      z
          // (x, y)
          e.Block(
              [ s.Let(
                    "x",
                    e.If(
                        e.Bool true,
                        e.Block([ s.Let("y", e.Int 10) ], e.Var "y"),
                        e.Block([ s.Let("z", e.Int 20) ], e.Var "z")
                    )
                ) ],
              e.Tuple [ e.Var "x"; e.Var "y" ]
          ),
          "Variable not found: y" ]
        |> List.iter (this.TestInvalidExpression prelude)


    [<TestMethod>]
    member this.TestCaptureVariables() =
        let prelude = ClassicValueContext.Prelude

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
          e.Block(
              [ s.Let("alpha", e.Bool true)
                s.Let("beta", e.Bool false)
                s.Let(
                    "foo",
                    e.Method(
                        arguments = [ "beta", Type.Int ],
                        returns = (Type.Tuple [ Type.Int; Type.Int; Type.Int ]),
                        body =
                            e.Block(
                                [ s.Let("alpha", e.If(e.Var "alpha", e.Multiply(e.Int 3, e.Var "beta"), e.Int 0))
                                  s.Let(
                                      "x",
                                      e.Block(
                                          [ s.Let("beta", e.Int 11)
                                            s.Let("alpha", e.Multiply(e.Var "alpha", e.Var "beta")) ],
                                          e.Var "alpha"
                                      )
                                  ) ],
                                e.Tuple [ e.Var "alpha"; e.Var "beta"; e.Var "x" ]
                            )
                    )
                ) ],
              e.Join(e.Tuple [ e.Var "alpha"; e.Var "beta" ], e.CallMethod(e.Var "foo", [ e.Int 25 ]))
          ),
          Tuple [ Bool true; Bool false; Value.Int 75; Value.Int 25; Value.Int 825 ]

          ]
        |> List.iter (this.TestExpression prelude)

    [<TestMethod>]
    member this.TestRecursiveMethod() =
        let prelude = ClassicValueContext.Prelude

        [
          // let sum (set:Set<Int>) =
          //      if Count(set) == 0 then
          //          0
          //      else
          //          let elem = Element(set)
          //          let rest = Remove(elem, set)
          //          elem + sum(rest)
          // sum( 1 .. 10 )
          e.Block(
              [ s.Let(
                    "sum",
                    e.Method(
                        arguments = [ ("set", (Type.Set Type.Int)) ],
                        returns = Type.Int,
                        body =
                            e.If(
                                (e.Equals(e.Count(e.Var "set"), e.Int 0),
                                 e.Int 0,
                                 e.Block(
                                     [ s.Let("elem", e.Element(e.Var "set"))
                                       s.Let("rest", e.Remove(e.Var "elem", e.Var "set")) ],
                                     e.Add(e.Var "elem", e.CallMethod(e.Var "sum", [ e.Var "rest" ]))
                                 ))
                            )
                    )
                ) ],
              e.CallMethod(e.Var "sum", [ e.Range(e.Int 1, e.Int 10) ])
          ),
          Int 45 ]
        |> List.iter (this.TestExpression prelude)
