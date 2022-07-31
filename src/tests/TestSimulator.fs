namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.parser.ast
open aleph.parser.TypeChecker
open aleph.runtime.Eval
open aleph.runtime.simulator

[<TestClass>]
(*
    These test take an untyped quantum (ket) expression, and
    prepares the simulator with the resulting Ket; they
    then verify that the quantum state and the returned columns
    from the preparation matches some expected values.
*)
type TestSimulator () =
    member this.Context = { ClassicValueContext.ctx with qpu = Simulator() }

    [<TestMethod>]
    member this.TestBasicExpressions () =
        let ctx = this.Context

        [
            // | false >
            u.Ket [ u.Bool false ], 
                [ [ Bool false ] ],
                [ 0 ]
            // | 0, 1, 2 >
            u.Ket [u.Int 0; u.Int 1; u.Int 2],
                [
                    [ Int 0 ]
                    [ Int 1 ]
                    [ Int 2 ]
                ],
                [ 0 ]
            // | (0,0), (0,1), (1,1) >
            u.Ket [u.Tuple [u.Int 0; u.Int 0]; u.Tuple [u.Int 0; u.Int 1]; u.Tuple [u.Int 1; u.Int 1]],
                [
                    [ Int 0; Int 0 ]
                    [ Int 0; Int 1 ]
                    [ Int 1; Int 1 ]
                ],
                [ 0; 1 ]
            // | (0,0,0), (0,1,1), (1,1,0), (1,1,2) >.2
            u.Project (u.Ket [
                    u.Tuple [ u.Int 0; u.Int 0; u.Int 0 ]
                    u.Tuple [ u.Int 0; u.Int 1; u.Int 1 ]
                    u.Tuple [ u.Int 1; u.Int 1; u.Int 0 ]
                    u.Tuple [ u.Int 1; u.Int 1; u.Int 2 ]
                ], u.Int 2),
                [
                    [ Int 0; Int 0; Int 0 ]
                    [ Int 0; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 0 ]
                    [ Int 1; Int 1; Int 2 ]
                ],
                [2]
            // ( | 0, 1 >, | 1, 2 > )
            u.Join (u.Ket [u.Int 0; u.Int 1], u.Ket [u.Int 1; u.Int 2]),
                [
                    [ Int 0; Int 1 ]
                    [ Int 0; Int 2 ]
                    [ Int 1; Int 1 ]
                    [ Int 1; Int 2 ]
                ],
                [ 0; 1 ]
        ]
        |> List.iter (this.TestExpression ctx)

        
    [<TestMethod>]
    member this.TestAddMultiply () =
        let ctx = 
            this.Context 
            |> this.AddToContext "k1" (AnyType.QType (QType.Ket [Type.Int; Type.Int])) (u.Ket [
                u.Tuple [ u.Int 0; u.Int 0]
                u.Tuple [ u.Int 0; u.Int 1]
                u.Tuple [ u.Int 1; u.Int 1]
            ])

        [
            // k1.0 + k1.1
            u.Add( u.Project (u.Var "k1", u.Int 0), u.Project (u.Var "k1", u.Int 1) ),
                [
                    [ Int 0; Int 0; Int 0 ]
                    [ Int 0; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 2 ]
                ],
                [ 2 ]
            // k1.0 + 1
            u.Add( u.Project (u.Var "k1", u.Int 0), u.Int 1),
                [
                    [ Int 0; Int 0; Int 1; Int 1 ]
                    [ Int 0; Int 1; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 1; Int 2 ]
                ],
                [ 3 ]
            // k1.0 * 5
            u.Multiply( u.Project (u.Var "k1", u.Int 0), u.Int 5),
                [
                    [ Int 0; Int 0; Int 5; Int 0 ]
                    [ Int 0; Int 1; Int 5; Int 0 ]
                    [ Int 1; Int 1; Int 5; Int 5 ]
                ],
                [ 3 ]
            // k1.0 + | 1, 2, 3 >
            u.Add(u.Project (u.Var "k1", u.Int 1), u.Ket [u.Int 1; u.Int 2; u.Int 3]),
                [
                    [ Int 0; Int 0; Int 1; Int 1 ]
                    [ Int 0; Int 0; Int 2; Int 2 ]
                    [ Int 0; Int 0; Int 3; Int 3 ]
                    [ Int 0; Int 1; Int 1; Int 2 ]
                    [ Int 0; Int 1; Int 2; Int 3 ]
                    [ Int 0; Int 1; Int 3; Int 4 ]
                    [ Int 1; Int 1; Int 1; Int 2 ]
                    [ Int 1; Int 1; Int 2; Int 3 ]
                    [ Int 1; Int 1; Int 3; Int 4 ]
                ],
                [ 3 ]
            // Join (k1.0, k1.1 + | 1, 2, 3 >)
            u.Join (u.Project (u.Var "k1", u.Int 0), u.Add(u.Project (u.Var "k1", u.Int 1), u.Ket [u.Int 1; u.Int 2; u.Int 3])),
                [
                    [ Int 0; Int 0; Int 1; Int 1 ]
                    [ Int 0; Int 0; Int 2; Int 2 ]
                    [ Int 0; Int 0; Int 3; Int 3 ]
                    [ Int 0; Int 1; Int 1; Int 2 ]
                    [ Int 0; Int 1; Int 2; Int 3 ]
                    [ Int 0; Int 1; Int 3; Int 4 ]
                    [ Int 1; Int 1; Int 1; Int 2 ]
                    [ Int 1; Int 1; Int 2; Int 3 ]
                    [ Int 1; Int 1; Int 3; Int 4 ]
                ],
                [ 0; 3 ]
            // Join (k1.0, k1.1 * | 1, 2, 3 >)
            u.Join (u.Project (u.Var "k1", u.Int 0), u.Multiply(u.Project (u.Var "k1", u.Int 1), u.Ket [u.Int 1; u.Int 2; u.Int 3])),
                [
                    [ Int 0; Int 0; Int 1; Int 0 ]
                    [ Int 0; Int 0; Int 2; Int 0 ]
                    [ Int 0; Int 0; Int 3; Int 0 ]
                    [ Int 0; Int 1; Int 1; Int 1 ]
                    [ Int 0; Int 1; Int 2; Int 2 ]
                    [ Int 0; Int 1; Int 3; Int 3 ]
                    [ Int 1; Int 1; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 2; Int 2 ]
                    [ Int 1; Int 1; Int 3; Int 3 ]
                ],
                [ 0; 3 ]
        ]
        |> List.iter (this.TestExpression ctx)

    [<TestMethod>]
    member this.TestJoin () =
        let ctx = 
            this.Context
            |> this.AddToContext "k1" (AnyType.QType (QType.Ket [Type.Int; Type.Int])) (u.Ket [
                u.Tuple [ u.Int 0; u.Int 0]
                u.Tuple [ u.Int 0; u.Int 1]
                u.Tuple [ u.Int 1; u.Int 1]
            ])
            |> this.AddToContext "k2" (AnyType.QType (QType.Ket [Type.Int])) (u.Ket [
                u.Tuple [ u.Int 1 ]
                u.Tuple [ u.Int 3 ]
            ])

        [
            // Join (k1, k1)
            u.Join (u.Var "k1", u.Var "k1"),
                [
                    [ Int 0; Int 0]
                    [ Int 0; Int 1]
                    [ Int 1; Int 1]
                ],
                [ 0; 1; 0; 1 ]
            // Join (k1, k2)
            u.Join (u.Var "k1", u.Var "k2"),
                [
                    [ Int 0; Int 0; Int 1 ]
                    [ Int 0; Int 0; Int 3 ]
                    [ Int 0; Int 1; Int 1 ]
                    [ Int 0; Int 1; Int 3 ]
                    [ Int 1; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 3 ]
                ],
                [ 0; 1; 2 ]
            // (Join k1, |true, false>)
            u.Join (u.Var "k1", u.Ket [u.Bool true; u.Bool false]),
                [
                    [ Int 0; Int 0; Bool false ]
                    [ Int 0; Int 0; Bool true ]
                    [ Int 0; Int 1; Bool false ]
                    [ Int 0; Int 1; Bool true ]
                    [ Int 1; Int 1; Bool false ]
                    [ Int 1; Int 1; Bool true ]
                ],
                [ 0; 1; 2 ]
            // let e1 = k1.1 + 10
            // let e2 = k2 + 10
            // (Join k1, e2 == e1 )
            u.Block (
                [
                    Let ("e1", u.Add (u.Project (u.Var "k1", u.Int 1), u.Int 10))
                    Let ("e2", u.Add (u.Var "k2", u.Int 10))
                ], u.Join (u.Var "k1", u.Equals(u.Var "e2", u.Var "e1"))),
                [
                    [ Int 0; Int 0; Int 1; Int 10; Int 11; Int 10; Int 10; Bool false ]
                    [ Int 0; Int 0; Int 3; Int 10; Int 13; Int 10; Int 10; Bool false ]
                    [ Int 0; Int 1; Int 1; Int 10; Int 11; Int 10; Int 11; Bool true ]
                    [ Int 0; Int 1; Int 3; Int 10; Int 13; Int 10; Int 11; Bool false ]
                    [ Int 1; Int 1; Int 1; Int 10; Int 11; Int 10; Int 11; Bool true ]
                    [ Int 1; Int 1; Int 3; Int 10; Int 13; Int 10; Int 11; Bool false ]
                ],
                [ 0; 1; 7 ]
        ]
        |> List.iter (this.TestExpression ctx)


    [<TestMethod>]
    member this.TestIndex () =
        let ctx = 
            this.Context 
            |> this.AddToContext "k1" (AnyType.QType (QType.Ket [Type.Int; Type.Int])) (u.Ket [
                u.Tuple [ u.Int 0; u.Int 0]
                u.Tuple [ u.Int 0; u.Int 1]
                u.Tuple [ u.Int 1; u.Int 1]
            ])

        [
            // k1.0 + k1.[0 + 1]
            u.Add( u.Project (u.Var "k1", u.Int 0), u.Project (u.Var "k1", u.Add (u.Int 0, u.Int 1))),
                [
                    [ Int 0; Int 0; Int 0 ]
                    [ Int 0; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 2 ]
                ],
                [ 2 ]
            // k1.0 + k1.[0 + 3]        // Index is modular, so 3 == 1
            u.Add( u.Project (u.Var "k1", u.Int 0), u.Project (u.Var "k1", u.Add (u.Int 0, u.Int 3))),
                [
                    [ Int 0; Int 0; Int 0 ]
                    [ Int 0; Int 1; Int 1 ]
                    [ Int 1; Int 1; Int 2 ]
                ],
                [ 2 ]
        ]
        |> List.iter (this.TestExpression ctx)

    [<TestMethod>]
    member this.TestIfQ () =
        let ctx = 
            this.Context
            |> this.AddToContext "k1" (AnyType.QType (QType.Ket [Type.Int; Type.Int])) (u.Ket [
                u.Tuple [ u.Int 0; u.Int 0]
                u.Tuple [ u.Int 0; u.Int 1]
                u.Tuple [ u.Int 1; u.Int 1]
            ])
            |> this.AddToContext "k2" (AnyType.QType (QType.Ket [Type.Int])) (u.Ket [
                u.Tuple [ u.Int 2 ]
                u.Tuple [ u.Int 3 ]
            ])

        [
            // if k1.1 == k1.0 then k1.1 else 42
            u.If (u.Equals ( u.Project (u.Var "k1", u.Int 1), u.Project (u.Var "k1", u.Int 0)),
                    u.Project (u.Var "k1", u.Int 1),
                    u.Int 42),
                [
                    [ Int 0; Int 0; Bool true; Int 42; Int 0 ]
                    [ Int 0; Int 1; Bool false; Int 42; Int 42 ]
                    [ Int 1; Int 1; Bool true; Int 42; Int 1 ]
                ],
                [ 4 ]
        ]
        |> List.iter (this.TestExpression ctx)

    [<TestMethod>]
    member this.TestIfClassic () =
        let ctx = 
            this.Context
            |> this.AddToContext "k1" (AnyType.QType (QType.Ket [Type.Int; Type.Int])) (u.Ket [
                u.Tuple [ u.Int 0; u.Int 0]
                u.Tuple [ u.Int 0; u.Int 1]
                u.Tuple [ u.Int 1; u.Int 1]
            ])
            |> this.AddToContext "k2" (AnyType.QType (QType.Ket [Type.Int])) (u.Ket [
                u.Tuple [ u.Int 2 ]
                u.Tuple [ u.Int 3 ]
            ])

        [
            // if true then k1.1 else 42
            u.If (u.Bool true,
                    u.Project (u.Var "k1", u.Int 1),
                    u.Int 42),
                [
                    [ Int 0; Int 0 ]
                    [ Int 0; Int 1 ]
                    [ Int 1; Int 1 ]
                ],
                [ 1 ]
            // if false then k1.1 else 42
            u.If (u.Bool false,
                    u.Project (u.Var "k1", u.Int 1),
                    u.Int 42),
                [
                    [ Int 42 ]
                ],
                [ 0 ]
        ]
        |> List.iter (this.TestExpression ctx)


    [<TestMethod>]
    member this.TestSolveEquals () =
        let ctx = 
            this.Context
            |> this.AddToContext "k1" (AnyType.QType (QType.Ket [Type.Int; Type.Int])) (u.Ket [
                u.Tuple [ u.Int 0; u.Int 0]
                u.Tuple [ u.Int 0; u.Int 1]
                u.Tuple [ u.Int 1; u.Int 1]
            ])
            |> this.AddToContext "k2" (AnyType.QType (QType.Ket [Type.Int])) (u.Ket [
                u.Tuple [ u.Int 2 ]
                u.Tuple [ u.Int 3 ]
            ])

        [
            // k1.1 == 1
            u.Equals ( u.Project (u.Var "k1", u.Int 1), u.Int 1),
                [
                    [ Int 0; Int 0; Int 1; Bool false ]
                    [ Int 0; Int 1; Int 1; Bool true ]
                    [ Int 1; Int 1; Int 1; Bool true ]
                ],
                [ 3 ]
            // (Solve k1, k1.1 == 1)
            u.Solve (u.Var "k1", u.Equals ( u.Project (u.Var "k1", u.Int 1), u.Int 1)),
                [
                    [ Int 0; Int 1; Int 1; Bool true ]
                    [ Int 1; Int 1; Int 1; Bool true ]
                ],
                [ 0; 1 ]
            // (Solve k1, k1.0 + k1.1 == 1)
            u.Solve (u.Var "k1", u.Equals ( u.Add( u.Project (u.Var "k1", u.Int 0), u.Project (u.Var "k1", u.Int 1) ), u.Int 1)),
                [
                    [ Int 0; Int 1; Int 1; Int 1; Bool true ]
                ],
                [ 0; 1 ]
            // (Solve k1.0, k1.1 + | 1, 2, 3 > == |2, 4> )
            u.Solve (u.Project (u.Var "k1", u.Int 1), u.Equals(u.Add(u.Project (u.Var "k1", u.Int 1), u.Ket [u.Int 1; u.Int 2; u.Int 3]), u.Ket [u.Int 2; u.Int 4])),
                [
                    [ Int 0; Int 0; Int 2; Int 2; Int 2; Bool true ]
                    [ Int 0; Int 1; Int 1; Int 2; Int 2; Bool true ]
                    [ Int 0; Int 1; Int 3; Int 4; Int 4; Bool true ]
                    [ Int 1; Int 1; Int 1; Int 2; Int 2; Bool true ]
                    [ Int 1; Int 1; Int 3; Int 4; Int 4; Bool true ]
                ],
                [ 1 ]
        ]
        |> List.iter (this.TestExpression ctx)


    [<TestMethod>]
    member this.TestBoolOps () =
        let ctx = 
            this.Context 
            |> this.AddToContext "k" (AnyType.QType (QType.Ket [Type.Int; Type.Bool])) (u.Ket [
                u.Tuple [ u.Int 0; u.Bool true]
                u.Tuple [ u.Int 0; u.Bool false]
                u.Tuple [ u.Int 1; u.Bool true]
            ])

        [
            u.Var "k",
                [
                    // Looks like because they are set, they are ordered differently from inputs:
                    // this might be problematic for tests...
                    [ Int 0; Bool false; ]
                    [ Int 0; Bool true; ]
                    [ Int 1; Bool true; ]
                ],
                [ 0; 1 ]

            // not k.1
            u.Not ( u.Project (u.Var "k", u.Int 1)),
                [
                    [ Int 0; Bool false; Bool true ]
                    [ Int 0; Bool true; Bool false ]
                    [ Int 1; Bool true; Bool false ]
                ],
                [ 2 ]

            // (false or k.1)
                u.Or ( 
                    u.Bool false,
                    u.Project (u.Var "k", u.Int 1)),
                [
                    [ Bool false; Int 0; Bool false; Bool false ]
                    [ Bool false; Int 0; Bool true;  Bool true ]
                    [ Bool false; Int 1; Bool true;  Bool true ]
                ],
                [ 3 ]

            // not (k.0 == 0 and k.1)
            u.Not ( 
                u.And ( 
                    u.Equals ( 
                        u.Project (u.Var "k", u.Int 0), 
                        u.Int 0), 
                    u.Project (u.Var "k", u.Int 1)) ),
                [
                    [ Int 0; Bool false; Int 0; Bool true; Bool false; Bool true ]
                    [ Int 0; Bool true;  Int 0; Bool true; Bool true; Bool false ]
                    [ Int 1; Bool true;  Int 0; Bool false; Bool false; Bool true ]
                ],
                [ 5 ]
        ]
        |> List.iter (this.TestExpression ctx)


    member this.TestExpression (ctx: ValueContext) (e, state, columns)=
        let qpu = ctx.qpu
        let sim = qpu :?> Simulator

        qpu.Reset()

        match run(e, ctx) with
        | Ok (Ket k, ctx) ->
            match qpu.Prepare (k, ctx) with
            | Ok (k', _) -> 
                let memory = sim.Memory
                let columns' = sim.Memory.allocations.[k'.Id]
                printfn "columns: %A\nmemory: %A\n" columns' memory
                Assert.AreEqual(state, memory.state)
                Assert.AreEqual(columns, columns')
            | Error msg -> 
                printfn "e: %A" e
                Assert.AreEqual($"Expecting Prepare Ok.", $"Got Error: {msg}")
        | Ok (v, _) ->
            printfn "e: %A" e
            Assert.AreEqual($"Expecting Ket value.", $"Got {v}")
        | Error msg ->
            printfn "e: %A" e
            Assert.AreEqual($"Expecting valid expression.", $"Got Error msg: {msg}")


    member this.TestInvalidExpression ctx (e, error) =
        match run (e, ctx) with
        | Ok (v, _) ->
            Assert.AreEqual($"Expected error: {error}", $"Got Value: {v}")
        | Error msg -> 
            Assert.AreEqual(error, msg)


    member this.AddToContext id t e ctx =
        match run (e, ctx) with
        | Ok (v, ctx) ->
            { ctx with heap = ctx.heap.Add (id, v); types = ctx.types.Add(id, t)  }
        | Error msg ->
            failwith msg
