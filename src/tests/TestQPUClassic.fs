namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.kets
open aleph.qpu.classic

open aleph.quals.tests.Utils

[<TestClass>]
(*
    Most of these tests take a Ket expression, and
    prepares it with the classical processor; they
    then verify that the returned Universe (its state/filters/outputs)
    from the preparation matches some expected value.
*)
type TestQPUClassic() =

    member this.QPU = Processor()

    [<TestMethod>]
    member this.TestBasicExpressions() =
        let a = Ket(Literal 2)
        let b = Ket(Literal 2)

        [ [ Ket(Literal 1) ], [ [ 0 ]; [ 1 ] ]

          [ a ], [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ] ]

          [ b ], [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ] ]

          [ a; a ], [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ] ]

          [ a; b ],
          [ [ 0; 0 ]
            [ 0; 1 ]
            [ 0; 2 ]
            [ 0; 3 ]
            [ 1; 0 ]
            [ 1; 1 ]
            [ 1; 2 ]
            [ 1; 3 ]
            [ 2; 0 ]
            [ 2; 1 ]
            [ 2; 2 ]
            [ 2; 3 ]
            [ 3; 0 ]
            [ 3; 1 ]
            [ 3; 2 ]
            [ 3; 3 ] ]


          [ a; b.Where(LessThanEquals, 2) ],
          [ [ 0; 0; 2; 1 ]
            [ 0; 1; 2; 1 ]
            [ 0; 2; 2; 1 ]
            [ 1; 0; 2; 1 ]
            [ 1; 1; 2; 1 ]
            [ 1; 2; 2; 1 ]
            [ 2; 0; 2; 1 ]
            [ 2; 1; 2; 1 ]
            [ 2; 2; 2; 1 ]
            [ 3; 0; 2; 1 ]
            [ 3; 1; 2; 1 ]
            [ 3; 2; 2; 1 ] ]

          [ a; Ket(Literal 1) ],
          [ [ 0; 0 ]; [ 0; 1 ]; [ 1; 0 ]; [ 1; 1 ]; [ 2; 0 ]; [ 2; 1 ]; [ 3; 0 ]; [ 3; 1 ] ]

          [ Ket(Literal 1); Ket(Constant 4) ],
          [ [ 0; 4 ]; [ 1; 4 ] ]
        ]
        |> List.iter (this.TestExpression)

    [<TestMethod>]
    member this.TestAddMultiply() =
        let a = Ket(Literal 1)
        let b = Ket(Literal 1)

        let c =
            Ket(Literal 2)
                .Multiply(3)
                .Where(LessThanEquals, Ket(Constant 3))
                .Add(b, width = 4)

        let d =
            Ket(Literal 2)
                .Multiply(3, width = 4)
                .Add(b)
                .Where(LessThanEquals, Ket(Constant 5))

        let e = c.Add(a.Where(Not))

        [ [ Ket(Map(Add(1), [ a; b ])) ],
          [ [ 0; 0; 0 ]; [ 0; 1; 1 ]; [ 1; 0; 1 ]; [ 1; 1; 0 ] ]

          [ a.Add(b) ],
          [ [ 0; 0; 0 ]; [ 0; 1; 1 ]; [ 1; 0; 1 ]; [ 1; 1; 0 ] ]

          [ Ket(Map(Add(2), [ a; b ])) ],
          [ [ 0; 0; 0 ]; [ 0; 1; 1 ]; [ 1; 0; 1 ]; [ 1; 1; 2 ] ]

          [ c ],
          [ [ 0; 3; 0; 3; 1; 0; 0 ]
            [ 0; 3; 0; 3; 1; 1; 1 ]
            [ 1; 3; 3; 3; 1; 0; 3 ]
            [ 1; 3; 3; 3; 1; 1; 4 ]
            [ 2; 3; 2; 3; 1; 0; 2 ]
            [ 2; 3; 2; 3; 1; 1; 3 ]
            [ 3; 3; 1; 3; 1; 0; 1 ]
            [ 3; 3; 1; 3; 1; 1; 2 ] ]

          [ d ],
          [ [ 0; 3; 0; 0; 0; 5; 1 ]
            [ 0; 3; 0; 1; 1; 5; 1 ]
            [ 1; 3; 3; 0; 3; 5; 1 ]
            [ 1; 3; 3; 1; 4; 5; 1 ] ]

          [ e ],
          [ [ 0; 3; 0; 3; 1; 0; 0; 0; 1; 0 ]
            [ 0; 3; 0; 3; 1; 1; 1; 0; 1; 1 ]
            [ 1; 3; 3; 3; 1; 0; 3; 0; 1; 3 ]
            [ 1; 3; 3; 3; 1; 1; 4; 0; 1; 4 ]
            [ 2; 3; 2; 3; 1; 0; 2; 0; 1; 2 ]
            [ 2; 3; 2; 3; 1; 1; 3; 0; 1; 3 ]
            [ 3; 3; 1; 3; 1; 0; 1; 0; 1; 1 ]
            [ 3; 3; 1; 3; 1; 1; 2; 0; 1; 2 ] ]
        ]
        |> List.iter (this.TestExpression)


    [<TestMethod>]
    member this.TestBoolean() =
        let a = Ket(Literal 1)
        let b = Ket(Literal 1)
        let c = a.Add(b, width = 2).Where(LessThanEquals, 1)

        // TODO: The compiler should know that
        //   d = a.LessThan(b)
        //   e = a.LessThan(b)
        // can point to the same ket.
        let d = a.LessThanEquals(b).And(b.LessThanEquals(1))
        let e = a.LessThanEquals(b).Or(b.LessThanEquals(1))
        let f = e.Not().Where(Equals, 1)

        [ [ a.LessThanEquals(b) ],
          [ [ 0; 0; 1 ]; [ 0; 1; 1 ]; [ 1; 0; 0 ]; [ 1; 1; 1 ] ]

          [ b.LessThanEquals(0) ], [ [ 0; 0; 1 ]; [ 1; 0; 0 ] ]

          [ c ],
          [ [ 0; 0; 0; 1; 1 ]; [ 0; 1; 1; 1; 1 ]; [ 1; 0; 1; 1; 1 ] ]

          [ d; e ],
          [ [ 0; 0; 1; 1; 1; 1; 1; 1; 1; 1 ]
            [ 0; 1; 1; 1; 1; 1; 1; 1; 1; 1 ]
            [ 1; 0; 0; 1; 1; 0; 0; 1; 1; 1 ]
            [ 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 ] ]

          [ f ],
          [ [ -1; -1; -1; -1; -1; -1; -1; -1; -1 ] ]
        ]
        |> List.iter (this.TestExpression)

    [<TestMethod>]
    member this.TestIn() =
        let a = Ket(Literal 2)

        [ [ a.In [ 0; 2 ] ],
          [ [ 0; 1 ]; [ 1; 0 ]; [ 2; 1 ]; [ 3; 0 ] ]

          [ a.Where(In [ 0; 2 ]) ],
          [ [ 0; 1 ]; [ 2; 1 ] ]

          [ a; a.Where(In []); a.In [ 1; 2; 3; 4 ] ],
          [ [ -1; -1; 0 ] ]

          [ a; a.Where(In []); Ket(Literal 2).In [ 1; 3 ] ],
          [ [ -1; -1; 0; 0 ]
            [ -1; -1; 1; 1 ]
            [ -1; -1; 2; 0 ]
            [ -1; -1; 3; 1 ] ]
        ]
        |> List.iter (this.TestExpression)

        
    [<TestMethod>]
    member this.TestId() =
        let a = Ket(Literal 2)

        [ [ a.Where(Id) ],
          [ [ 1 ]; [ 2 ]; [ 3 ] ]

          [ a; a.GreaterThan(2).Where(Id) ],
          [ [ 3; 2; 1 ] ]
        ]
        |> List.iter (this.TestExpression)


    [<TestMethod>]
    member this.TestChoose() =
        let a = Ket(Literal 2)
        let b = a.Add(4, width = 3)

        let c = a.In([ 1; 2 ]).Choose(a, b)

        [ [ c ],
          [ [ 0; 0; 4; 4; 4 ]; [ 1; 1; 4; 5; 1 ]; [ 2; 1; 4; 6; 2 ]; [ 3; 0; 4; 7; 7 ] ]
        ]
        |> List.iter (this.TestExpression)


    (*
        These test check that filter_rows collectly
        removes the rows that satisfy the filter columns.
    *)
    [<TestMethod>]
    member this.TestFilter() =
        let state =
            [ [ 0; 0; 2; 1; 0 ]
              [ 0; 1; 2; 1; 0 ]
              [ 0; 2; 2; 0; 0 ]
              [ 0; 3; 2; 0; 0 ]
              [ 1; 0; 2; 1; 0 ]
              [ 1; 1; 2; 1; 0 ]
              [ 1; 2; 2; 0; 0 ]
              [ 1; 3; 2; 0; 0 ]
              [ 2; 0; 2; 1; 0 ]
              [ 2; 1; 2; 1; 0 ]
              [ 2; 2; 2; 0; 0 ]
              [ 2; 3; 2; 0; 0 ]
              [ 3; 0; 2; 1; 0 ]
              [ 3; 1; 2; 1; 0 ]
              [ 3; 2; 2; 0; 0 ]
              [ 3; 3; 2; 0; 0 ] ] |> QuantumState

        let expected =
            [ [ 0; 0; 2; 1; 0 ]
              [ 0; 1; 2; 1; 0 ]
              [ 1; 0; 2; 1; 0 ]
              [ 1; 1; 2; 1; 0 ]
              [ 2; 0; 2; 1; 0 ]
              [ 2; 1; 2; 1; 0 ]
              [ 3; 0; 2; 1; 0 ]
              [ 3; 1; 2; 1; 0 ] ] |> QuantumState

        let actual = state.FilterRows 3
        Assert.AreEqual(expected.Rows, actual.Rows)

        let expected =
            [ [ 0; 1; 2; 1; 0 ]; [ 1; 1; 2; 1; 0 ]; [ 2; 1; 2; 1; 0 ]; [ 3; 1; 2; 1; 0 ] ] |> QuantumState

        let actual = (state.FilterRows 3).FilterRows 1
        Assert.AreEqual(expected.Rows, actual.Rows)

        let actual = (state.FilterRows 1).FilterRows 3
        Assert.AreEqual(expected.Rows, actual.Rows)

        let actual = (expected.FilterRows 3).FilterRows 1
        Assert.AreEqual(expected.Rows, actual.Rows)

        let expected = [ [ 1; 1; 2; 1; 0 ]; [ 2; 1; 2; 1; 0 ]; [ 3; 1; 2; 1; 0 ] ] |> QuantumState

        let actual = ((state.FilterRows 0).FilterRows 3).FilterRows 1
        Assert.AreEqual(expected.Rows, actual.Rows)

        let expected = [ [ -1; -1; -1; -1; -1 ] ] |> QuantumState
        let actual = ((((state.FilterRows 0).FilterRows 3).FilterRows 1).FilterRows 2).FilterRows 4
        Assert.AreEqual(expected.Rows, actual.Rows)

        let actual = (state.FilterRows 4)
        Assert.AreEqual(expected.Rows, actual.Rows)


    [<TestMethod>]
    member this.TestSample() =
        let a = Ket(Literal 2)
        let b = Ket(Literal 2)
        let c = a.Add(b).Where(LessThanEquals, 1)

        // TODO: The compiler should know that
        //   d = a.LessThan(b)
        //   e = a.LessThan(b)
        // can point to the same ket.
        let d = a.LessThanEquals(b).And(b.LessThanEquals(1))
        let e = a.LessThanEquals(b).Or(b.LessThanEquals(1))
        let f = e.Not().Where(Equals, 1)

        let u = (prepare { qpu = this.QPU } [ f ]) :?> Universe
        printf "%A" (u.State)

        [ [ a ], [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ] ]

          [ b.Where(LessThanEquals, 2) ], [ [ 0 ]; [ 1 ]; [ 2 ] ]

          [ a; b.Where(LessThanEquals, 1) ],
          [ [ 0; 0 ]; [ 0; 1 ]; [ 1; 0 ]; [ 1; 1 ]; [ 2; 0 ]; [ 2; 1 ]; [ 3; 0 ]; [ 3; 1 ] ]

          [ a; b; a.LessThanEquals(b) ],
          [ [ 0; 0; 1 ]
            [ 0; 1; 1 ]
            [ 0; 2; 1 ]
            [ 0; 3; 1 ]
            [ 1; 0; 0 ]
            [ 1; 1; 1 ]
            [ 1; 2; 1 ]
            [ 1; 3; 1 ]
            [ 2; 0; 0 ]
            [ 2; 1; 0 ]
            [ 2; 2; 1 ]
            [ 2; 3; 1 ]
            [ 3; 0; 0 ]
            [ 3; 1; 0 ]
            [ 3; 2; 0 ]
            [ 3; 3; 1 ] ]

          [ a; b; a.GreaterThan(1) ],
          [ [ 0; 0; 0 ]
            [ 0; 1; 0 ]
            [ 0; 2; 0 ]
            [ 0; 3; 0 ]
            [ 1; 0; 0 ]
            [ 1; 1; 0 ]
            [ 1; 2; 0 ]
            [ 1; 3; 0 ]
            [ 2; 0; 1 ]
            [ 2; 1; 1 ]
            [ 2; 2; 1 ]
            [ 2; 3; 1 ]
            [ 3; 0; 1 ]
            [ 3; 1; 1 ]
            [ 3; 2; 1 ]
            [ 3; 3; 1 ] ]

          [ a.Where(GreaterThan, 1); b.Where(GreaterThan, a) ], [ [ 2; 3 ] ]

          [ a.Where(LessThanEquals, b); b ],
          [ [ 0; 0 ]; [ 0; 1 ]; [ 0; 2 ]; [ 0; 3 ]; [ 1; 1 ]; [ 1; 2 ]; [ 1; 3 ]; [ 2; 2 ]; [ 2; 3 ]; [ 3; 3 ] ]

          [ a; b; c ],
          [ [ 0; 0; 0 ]
            [ 0; 1; 1 ]
            [ 1; 0; 1 ]
            [ 1; 1; 2 ]
            [ 1; 3; 0 ]
            [ 2; 2; 0 ]
            [ 2; 3; 1 ]
            [ 3; 1; 0 ]
            [ 3; 2; 1 ] ]

          [ a; b; d; e ],
          [ [ 0; 0; 1; 1 ]
            [ 0; 1; 1; 1 ]
            [ 0; 2; 0; 1 ]
            [ 0; 3; 0; 1 ]
            [ 1; 0; 0; 1 ]
            [ 1; 1; 1; 1 ]
            [ 1; 2; 0; 1 ]
            [ 1; 3; 0; 1 ]
            [ 2; 0; 0; 1 ]
            [ 2; 1; 0; 1 ]
            [ 2; 2; 0; 1 ]
            [ 2; 3; 0; 1 ]
            [ 3; 0; 0; 1 ]
            [ 3; 1; 0; 1 ]
            [ 3; 2; 0; 0 ]
            [ 3; 3; 0; 1 ] ]

          [ a; f ], [ [ 3; 1 ] ] ]
        |> List.iter (AssertSample this.QPU)


    member this.TestExpression(exprs, expected) =
        printfn "expr: %A" exprs

        let ctx = { qpu = this.QPU }
        let actual = prepare ctx exprs :?> Universe

        printfn "state: %A\n" actual.State.Rows
        Assert.AreEqual(expected, actual.State.Rows)
