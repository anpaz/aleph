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


    member this.TestExpression (e, expected, ?ctx)=
        let ctx = defaultArg ctx Map.empty
        
        match eval (e, ctx) with
        | Ok (actual, _) -> 
            Assert.AreEqual(expected.ToString(), actual.ToString())
        | Error msg -> 
            Assert.AreEqual(expected, msg)
            Assert.Fail()
        
    member this.TestInvalidExpression (e, expected, ?ctx)=
        let ctx = defaultArg ctx Map.empty
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

        Map[ 
            ("i1", i1)
            ("b1", b1)
            ("t1", t1)
            ("t2", t2)
            ("s1", s1)
            ("k1", k1)
        ]

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

