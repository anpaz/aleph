namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.runtime.Core
open aleph.runtime.Classic

open aleph.parser.core
open aleph.parser.quantum


[<TestClass>]
type TestClassic () =

    let Measure = Measure >> Q
    let Ket = Ket >> Q
    let Solve = Solve >> Q

    member this.TestExpression (e, expected, ctx)=        
        match eval (e, ctx) with
        | Ok (actual, _) -> 
            Assert.AreEqual(expected.ToString(), actual.ToString())
        | Error msg -> 
            Assert.AreEqual(expected, msg)
            Assert.Fail()
        
    member this.TestInvalidExpression (e, expected, ctx)=
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
        let k1 = Value.Q (K ( SET [
            [I  0; I  1; I  3; B false; I  5]
            [I 10; I 11; I 13; B true;  I 25]
        ]))

        Context (Map[ 
            ("i1", i1)
            ("b1", b1)
            ("t1", t1)
            ("t2", t2)
            ("s1", s1)
            ("k1", k1)
        ])

    [<TestMethod>]
    member this.KetExpressions() =
        let ctx = this.Context

        // ---------------------------------- //
        // Positive cases
        // ---------------------------------- //
        [
            // |>
            (Ket [], Value.Q (K (SET [])))
            
            // |[], []> -> |>
            (Ket [
                Set([])
                Set([])
            ], Value.Q (K (SET [])))
            
            // |(3, 4)> -> |(3, 4)>
            (Ket [
                Tuple([Int(3); Int(4)])
            ], Value.Q (K (SET [[I 3; I 4]])))

            // |0> -> |0>
            (Ket [Int(0)]), Value.Q (K (SET[
                [I 0]
            ]))
            
            // |1, 2, 1> --> |(1), (2)>
            (Ket [Int(1); Int(2); Int(1)]), Value.Q (K (SET[
                [I 1]
                [I 2]
            ]))
            
            //| [ (1,t), (3,f) ] > --> | (1,t), (3,f) >
            (Ket [ Set [
                Tuple[Int(1); Bool(true)]
                Tuple[Int(3); Bool(false)]]
            ], Value.Q (K ( SET [
                [I 1 ; B true]
                [I 3 ; B false]
            ])))

            // | s1 > --> | (5, 12) >
            (Ket [ Id "s1"]), Value.Q (K (SET[
                [B false; I 5]
                [B true; I 12]
            ]))

            // ( | (0,0,0), (1,1,1) >, | (0,1,0), (1,1,1) >  ) --> | (0,0,0,0,1,0), (0,0,0,0,1,0), (1,1,1,0,1,0), (1,1,1,1,1,1) >
            (Tuple([
                Ket [
                    Tuple([Int(0); Int(0); Int(0)])
                    Tuple([Int(1); Int(1); Int(1)])
                ]
                Ket [
                    Tuple([Int(0); Int(1); Int(0)])
                    Tuple([Int(1); Int(1); Int(1)])
                ]
            ]), Value.Q (K ( SET([
                [I(0); I(0); I(0); I(0); I(1); I(0)]
                [I(0); I(0); I(0); I(1); I(1); I(1)]
                [I(1); I(1); I(1); I(0); I(1); I(0)]
                [I(1); I(1); I(1); I(1); I(1); I(1)]
            ]))))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))


        // ---------------------------------- //
        // Negative cases
        // ---------------------------------- //
        [
            //  given k1 = | (F, 5), (T,12) >
            // | k1 >
            (Ket [Id "k1"], "Invalid value for a set element: | (0, 1, 3, False, 5), (10, 11, 13, True, 25) >")

            //| | 1, 2> >: Ket of ket
            (Ket [ Ket [
                    Int(1)
                    Int(2)]
            ], "Invalid value for a set element: | 1, 2 >")

            //[ | 1, 2> ] : Set of kets
            (Set [
                Ket [
                    Int(1)
                    Int(2) ]
            ], "Invalid value for a set element: | 1, 2 >")

        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))


    [<TestMethod>]
    member this.MeasureExpressions() =
        let ctx = this.Context

        // ---------------------------------- //
        // Positive cases
        // ---------------------------------- //
        [
            // | |> | -> ()
            (Measure (Ket []), Value.Tuple [])
            
            // | |(3, 4)> | -> (3, 4)
            (Measure (Ket [
                Tuple([Int(3); Int(4)])
            ]), Value.Tuple [I 3; I 4])

            // |0> -> 0
            (Measure (Ket [Int(0)]), Value.Int 0)
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        // | ( | (0,0,0), (1,1,1) >, | (0,1,0), (1,1,1) >  ) | --> one of (0,0,0,0,1,0), (0,0,0,0,1,0), (1,1,1,0,1,0), (1,1,1,1,1,1) >
        let e = Measure ( 
            Tuple [
                Ket [
                    Tuple([Int(0); Int(0); Int(0)])
                    Tuple([Int(1); Int(1); Int(1)])]
                Ket [
                    Tuple([Int(0); Int(1); Int(0)])
                    Tuple([Int(1); Int(1); Int(1)])]])

        match eval (e, ctx) with
        | Ok (Value.Tuple [I(0); I(0); I(0); I(0); I(1); I(0)], _) 
        | Ok (Value.Tuple [I(0); I(0); I(0); I(1); I(1); I(1)], _)
        | Ok (Value.Tuple [I(1); I(1); I(1); I(0); I(1); I(0)], _)
        | Ok (Value.Tuple [I(1); I(1); I(1); I(1); I(1); I(1)], _) -> Assert.IsTrue(true)
        | Ok v -> Assert.AreEqual("", $"Not expected value: {v}")
        | Error msg -> Assert.AreEqual("", msg)

        // ---------------------------------- //
        // Negative cases
        // ---------------------------------- //
        [
            // | [1, 2] |
            (Measure (Set [Int 1; Int 2]), "Measure not available for [1, 2]")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))


    [<TestMethod>]
    member this.SolveExpressions() =
        let ctx = this.Context

        [
            // Solve |(3, true), (4, false), (5, false))> -> |3>
            (Solve (Ket [
                Tuple [Int 3; Bool true]
                Tuple [Int 4; Bool false]
                Tuple [Int 5; Bool false]
            ]), Value.Q (K (SET [ [I 3] ])))
            // Solve |(3, true), (4, true), (5, true))> -> |3, 4, 5>
            (Solve(Ket [
                Tuple [Int 3; Bool true]
                Tuple [Int 4; Bool true]
                Tuple [Int 5; Bool true]
            ]), Value.Q (K (SET [ [I 3]; [I 4]; [I 5] ])))
            // Solve |(1, 1, true), (2, 4, false), (3, 5, true))> -> |(1, 1), (1, 5)>
            (Solve (Ket [
                Tuple [Int 1; Int 1; Bool true]
                Tuple [Int 2; Int 4; Bool false]
                Tuple [Int 3; Int 5; Bool true]
            ]), Value.Q (K (SET [ [I 1; I 1]; [I 3; I 5] ])))
            // Solve |(1, 1, 13, 0) (2, 4, 24, -1), (3, 5, 35, 1))> -> |(2, 4, 24)>
            (Solve (Ket [
                Tuple [Int 1; Int 1; Int 13; Int 0]
                Tuple [Int 2; Int 4; Int 24; Int -1]
                Tuple [Int 3; Int 5; Int 35; Int 10]
                Tuple [Int 3; Int 5; Int 35; Int 12]
                Tuple [Int 2; Int 4; Int 24; Int -1]
                Tuple [Int 3; Int 5; Int 12; Int 0]
                Tuple [Int 3; Int 5; Int 25; Int -1]
            ]), Value.Q (K (SET [ [I 2; I 4; I 24]; [I 3; I 5; I 25] ])))
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
    member this.CallQuantumExpressions() =
        let ctx = 
            this.Context
                .Add("void", 
                    Value.Q (U (
                        arguments=[], 
                        ket=[""], 
                        body=Tuple [])))
                .Add("colors", 
                    Value.Q (U  (
                        arguments=[],
                        ket=["k"],
                        body=(Tuple[Int 1;Int 2; Int 3]))))
                .Add("append", 
                    Value.Q (U  (
                        arguments=["a"],
                        ket=["k"],
                        body=(Id "a"))))
                .Add("shadow", 
                    Value.Q (U  (
                        arguments=["i1"],
                        ket=["k1"],
                        body=(Add [Id "k1"; Id "i1"]))))
                .Add("add", 
                    Value.Q (U  (
                        arguments=["a"], 
                        ket=["b"; "c"], 
                        body=(Add [Id "a"; Id "b"; Id "c"]))))
                .Add("qone-ket", 
                    Value.Q (U  (
                        arguments=["a"; "d"],
                        ket=["k1"], 
                        body=(Q (CallUnitary (
                                id="add",
                                arguments = [Add [Id "a"; Int 1]],
                                ket=(Id "k1")))))))
                .Add("qone-qreg", 
                    Value.Q (U  (
                        arguments=["a"; "d"],
                        ket=["k1"], 
                        body=(Q (CallUnitary (
                                id="add",
                                arguments = [Add [Id "a"; Int 1]],
                                ket=Project (Id "k1", [Int 0; Int 2])))))))
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
            // void() | -> ()
            (Q (CallUnitary("void", [], empty)), Value.Q (K (SET [])))
            // void() k1 -> |0>
            (Q (CallUnitary("void", [], k1)), Value.Q (K (SET [[I 0]])))
            
            // colors | empty -> |>
            (Q (CallUnitary("colors", [], empty)), Value.Q (K  (SET [])))
            // colors | k1 -> |(0, 1, 2, 3)>
            (Q (CallUnitary("colors", [], k1)), Value.Q (K  (SET [[I 0; I 1; I 2; I 3]])))
            // colors | k3 -> |(0,1, 2, 3), (1, 1, 2, 3), (2, 1, 2, 3)>
            (Q (CallUnitary("colors", [], k3)), Value.Q (K  (SET [
                [I 0; I 1; I 2; I 3]
                [I 1; I 1; I 2; I 3]
                [I 2; I 1; I 2; I 3]
            ])))
            // colors | tuples -> |(0,1, 2, 3), (1, 1, 2, 3), (2, 1, 2, 3)>
            (Q (CallUnitary("colors", [], tuples)), Value.Q (K  (SET [
                [I 0; B true;  I 1; I 2; I 3]
                [I 1; B false; I 1; I 2; I 3]
                [I 2; B true;  I 1; I 2; I 3]
            ])))

            // add 10 | | (0,0), (1,1), (2,2) > --> |(0,0,10), (1,1,12), (2,2,14) >
            (Q (CallUnitary(
                    id="add",
                    arguments=[Int 10],
                    ket= Ket [Tuple [Int 0; Int 0]; Tuple [Int 1; Int 1]; Tuple [Int 2; Int 2]])), 
                Value.Q (K  (SET [
                    [I 0; I 0; I 10]
                    [I 1; I 1; I 12]
                    [I 2; I 2; I 14]])))

            // qone-ket 1 20 | | (0,0), (1,1), (2,2) > --> |(0,0,2), (1,1,4), (2,2,6) >
            (Q (CallUnitary(
                    id="qone-ket",
                    arguments=[Int 1; Int 20],
                    ket= Ket [
                        Tuple [Int 0; Int 0];
                        Tuple [Int 1; Int 1];
                        Tuple [Int 2; Int 2]])), 
                Value.Q (K  (SET [
                    [I 0; I 0; I 2]
                    [I 1; I 1; I 4]
                    [I 2; I 2; I 6]])))

            // qone-qreg (20,1) | | (0,0), (1,1), (2,2) > --> |(0,0,21), (1,1,23), (2,2,25) >
            (Q (CallUnitary(
                    id="qone-qreg",
                    arguments=[Tuple [Int 20; Int 21]],
                    ket= Ket [
                        Tuple [Int 0; Bool false; Int 0];
                        Tuple [Int 1; Bool false; Int 1]; 
                        Tuple [Int 2; Bool false; Int 2]])), 
                Value.Q (K  (SET [
                    [I 0; B false; I 0; I 21]
                    [I 1; B false; I 1; I 23]
                    [I 2; B false; I 2; I 25]])))
            
            // // shadow(10) k3 -> | (0,10), (1,11), (2,12) >
            // (CallQuantum("shadow", [Int 10], k3), Value.Ket (SET [[I 0; I 10]; [I 1; I 11]; [I 2; I 12]]))

            // // append(true) empty -> |>
            // (CallQuantum("append", [Bool true], empty), Value.Ket (SET []))
            // // append( (false, false) ) k1 -> |(0, false, false)>
            // (CallQuantum("append", [Tuple [Bool false; Bool false]], k1), Value.Ket (SET [
            //     [I 0; B false; B false]
            // ]))
            // // append( (false, false) ) k3 -> |(0, false, false), (1, false, false), (2, false, false)>
            // (CallQuantum("append", [Tuple [Bool false; Bool false]], k3), Value.Ket (SET [
            //     [I 0; B false; B false]
            //     [I 1; B false; B false]
            //     [I 2; B false; B false]
            // ]))
            // // append( [false, false, true] ) tuples -> |(0,true,false), (0,true,true), (1,false,false), (1,false, true), (2,true,false) (2,true,true)>
            // (CallQuantum("append", [Set [Bool false; Bool false; Bool true]], tuples), Value.Ket (SET [
            //     [I 0; B true; B false]
            //     [I 0; B true; B true]
            //     [I 1; B false; B false]
            //     [I 1; B false; B true]
            //     [I 2; B true; B false]
            //     [I 2; B true; B true]
            // ]))

            // // qone() k3 -> | (0,1), (1,2), (2,3) >
            // (CallQuantum("qone", [], k3), Value.Ket (SET [[I 0; I 1]; [I 1; I 2]; [I 2; I 3]]))

            // // qone() tuples -> | (0,true,1), (1,false,2), (2,true,3) >
            // (CallQuantum("qone", [], k3), Value.Ket (SET [[I 0; I 1]; [I 1; I 2]; [I 2; I 3]]))
        ]
        |> List.iter (fun (e, v) -> this.TestExpression (e, v, ctx))

        [
            // (CallQuantum("append", [], empty), "Invalid arguments: expects 1, got 0")
            // (CallQuantum("append", [Int 1; Int 1], empty), "Invalid arguments: expects 1, got 2")
            // (CallQuantum("foo", [Id "t1"], empty), "Undefined quantum: foo")
            // (CallQuantum("t1", [Id "i1"], empty), "Undefined quantum: t1")
            // (CallQuantum("append", [Id "i1"], Int 777), "Expecting ket value, got: 777")
        ]
        |> List.iter (fun (n, msg) -> this.TestInvalidExpression (n, msg, ctx))

