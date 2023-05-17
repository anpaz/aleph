namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.quals.parser.ast
open aleph.quals.runtime.Eval
open aleph.quals.runtime.qpu.classic

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

        [ [ Ket(Literal 1) ], Universe(state = [ [ 0 ]; [ 1 ] ], outputColumns = [ 0 ], filters = [])

          [ a ], Universe(state = [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ] ], outputColumns = [ 0 ], filters = [])

          [ b ], Universe(state = [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ] ], outputColumns = [ 0 ], filters = [])

          [ a; a ], Universe(state = [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ] ], outputColumns = [ 0; 0 ], filters = [])

          [ a; b ],
          Universe(
              state =
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
                    [ 3; 3 ] ],
              filters = [],
              outputColumns = [ 0; 1 ]
          )


          [ a; b.Where(LessThanEquals, 2) ],
          Universe(
              state =
                  [ [ 0; 0; 2; 1 ]
                    [ 0; 1; 2; 1 ]
                    [ 0; 2; 2; 1 ]
                    [ 0; 3; 2; 0 ]
                    [ 1; 0; 2; 1 ]
                    [ 1; 1; 2; 1 ]
                    [ 1; 2; 2; 1 ]
                    [ 1; 3; 2; 0 ]
                    [ 2; 0; 2; 1 ]
                    [ 2; 1; 2; 1 ]
                    [ 2; 2; 2; 1 ]
                    [ 2; 3; 2; 0 ]
                    [ 3; 0; 2; 1 ]
                    [ 3; 1; 2; 1 ]
                    [ 3; 2; 2; 1 ]
                    [ 3; 3; 2; 0 ] ],
              filters = [ 3 ],
              outputColumns = [ 0; 1 ]
          )

          [ a; Ket(Literal 1) ],
          Universe(
              state = [ [ 0; 0 ]; [ 0; 1 ]; [ 1; 0 ]; [ 1; 1 ]; [ 2; 0 ]; [ 2; 1 ]; [ 3; 0 ]; [ 3; 1 ] ],
              filters = [],
              outputColumns = [ 0; 1 ]
          )

          [ Ket(Literal 1); Ket(Constant 4) ],
          Universe(state = [ [ 0; 4 ]; [ 1; 4 ] ], outputColumns = [ 0; 1 ], filters = []) ]
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
          Universe(state = [ [ 0; 0; 0 ]; [ 0; 1; 1 ]; [ 1; 0; 1 ]; [ 1; 1; 0 ] ], outputColumns = [ 2 ], filters = [])

          [ a.Add(b) ],
          Universe(state = [ [ 0; 0; 0 ]; [ 0; 1; 1 ]; [ 1; 0; 1 ]; [ 1; 1; 0 ] ], outputColumns = [ 2 ], filters = [])

          [ Ket(Map(Add(2), [ a; b ])) ],
          Universe(state = [ [ 0; 0; 0 ]; [ 0; 1; 1 ]; [ 1; 0; 1 ]; [ 1; 1; 2 ] ], outputColumns = [ 2 ], filters = [])

          [ c ],
          Universe(
              state =
                  [ [ 0; 3; 0; 3; 1; 0; 0 ]
                    [ 0; 3; 0; 3; 1; 1; 1 ]
                    [ 1; 3; 3; 3; 1; 0; 3 ]
                    [ 1; 3; 3; 3; 1; 1; 4 ]
                    [ 2; 3; 2; 3; 1; 0; 2 ]
                    [ 2; 3; 2; 3; 1; 1; 3 ]
                    [ 3; 3; 1; 3; 1; 0; 1 ]
                    [ 3; 3; 1; 3; 1; 1; 2 ] ],
              outputColumns = [ 6 ],
              filters = [ 4 ]
          )

          [ d ],
          Universe(
              state =
                  [ [ 0; 3; 0; 0; 0; 5; 1 ]
                    [ 0; 3; 0; 1; 1; 5; 1 ]
                    [ 1; 3; 3; 0; 3; 5; 1 ]
                    [ 1; 3; 3; 1; 4; 5; 1 ]
                    [ 2; 3; 6; 0; 6; 5; 0 ]
                    [ 2; 3; 6; 1; 7; 5; 0 ]
                    [ 3; 3; 9; 0; 9; 5; 0 ]
                    [ 3; 3; 9; 1; 10; 5; 0 ] ],
              outputColumns = [ 4 ],
              filters = [ 6 ]
          )

          [ e ],
          Universe(
              state =
                  [ [ 0; 3; 0; 3; 1; 0; 0; 0; 1; 0 ]
                    [ 0; 3; 0; 3; 1; 0; 0; 1; 0; 1 ]
                    [ 0; 3; 0; 3; 1; 1; 1; 0; 1; 1 ]
                    [ 0; 3; 0; 3; 1; 1; 1; 1; 0; 2 ]
                    [ 1; 3; 3; 3; 1; 0; 3; 0; 1; 3 ]
                    [ 1; 3; 3; 3; 1; 0; 3; 1; 0; 4 ]
                    [ 1; 3; 3; 3; 1; 1; 4; 0; 1; 4 ]
                    [ 1; 3; 3; 3; 1; 1; 4; 1; 0; 5 ]
                    [ 2; 3; 2; 3; 1; 0; 2; 0; 1; 2 ]
                    [ 2; 3; 2; 3; 1; 0; 2; 1; 0; 3 ]
                    [ 2; 3; 2; 3; 1; 1; 3; 0; 1; 3 ]
                    [ 2; 3; 2; 3; 1; 1; 3; 1; 0; 4 ]
                    [ 3; 3; 1; 3; 1; 0; 1; 0; 1; 1 ]
                    [ 3; 3; 1; 3; 1; 0; 1; 1; 0; 2 ]
                    [ 3; 3; 1; 3; 1; 1; 2; 0; 1; 2 ]
                    [ 3; 3; 1; 3; 1; 1; 2; 1; 0; 3 ] ],
              outputColumns = [ 9 ],
              filters = [ 4; 8 ]
          ) ]
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
          Universe(state = [ [ 0; 0; 1 ]; [ 0; 1; 1 ]; [ 1; 0; 0 ]; [ 1; 1; 1 ] ], outputColumns = [ 2 ], filters = [])

          [ b.LessThanEquals(0) ], Universe(state = [ [ 0; 0; 1 ]; [ 1; 0; 0 ] ], outputColumns = [ 2 ], filters = [])

          [ c ],
          Universe(
              state = [ [ 0; 0; 0; 1; 1 ]; [ 0; 1; 1; 1; 1 ]; [ 1; 0; 1; 1; 1 ]; [ 1; 1; 2; 1; 0 ] ],
              outputColumns = [ 2 ],
              filters = [ 4 ]
          )

          [ d; e ],
          Universe(
              state =
                  [ [ 0; 0; 1; 1; 1; 1; 1; 1; 1; 1 ]
                    [ 0; 1; 1; 1; 1; 1; 1; 1; 1; 1 ]
                    [ 1; 0; 0; 1; 1; 0; 0; 1; 1; 1 ]
                    [ 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 ] ],
              outputColumns = [ 5; 9 ],
              filters = []
          )

          [ f ],
          Universe(
              state =
                  [ [ 0; 0; 1; 1; 1; 1; 0; 1; 0 ]
                    [ 0; 1; 1; 1; 1; 1; 0; 1; 0 ]
                    [ 1; 0; 0; 1; 1; 1; 0; 1; 0 ]
                    [ 1; 1; 1; 1; 1; 1; 0; 1; 0 ] ],
              outputColumns = [ 6 ],
              filters = [ 8 ]
          ) ]
        |> List.iter (this.TestExpression)

    [<TestMethod>]
    member this.TestIn() =
        let a = Ket(Literal 2)

        [ [ a.In [ 0; 2 ] ],
          Universe(state = [ [ 0; 1 ]; [ 1; 0 ]; [ 2; 1 ]; [ 3; 0 ] ], outputColumns = [ 1 ], filters = [])
          [ a.Where(In [ 0; 2 ]) ],
          Universe(state = [ [ 0; 1 ]; [ 1; 0 ]; [ 2; 1 ]; [ 3; 0 ] ], outputColumns = [ 0 ], filters = [ 1 ])

          [ a; a.Where(In []); a.In [ 1; 2; 3; 4 ] ],
          Universe(
              state = [ [ 0; 0; 0 ]; [ 1; 0; 1 ]; [ 2; 0; 1 ]; [ 3; 0; 1 ] ],
              outputColumns = [ 0; 0; 2 ],
              filters = [ 1 ]
          ) ]
        |> List.iter (this.TestExpression)


    [<TestMethod>]
    member this.TestChoose() =
        let a = Ket(Literal 2)
        let b = a.Add(4, width = 3)

        let c = a.In([ 1; 2 ]).Choose(a, b)

        [ [ c ],
          Universe(
              state = [ [ 0; 0; 4; 4; 4 ]; [ 1; 1; 4; 5; 1 ]; [ 2; 1; 4; 6; 2 ]; [ 3; 0; 4; 7; 7 ] ],
              outputColumns = [ 4 ],
              filters = []
          ) ]
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
              [ 3; 3; 2; 0; 0 ] ]

        let actual = Universe.filter_rows (state, [])
        Assert.AreEqual(state, actual)

        let expected =
            [ [ 0; 0; 2; 1; 0 ]
              [ 0; 1; 2; 1; 0 ]
              [ 1; 0; 2; 1; 0 ]
              [ 1; 1; 2; 1; 0 ]
              [ 2; 0; 2; 1; 0 ]
              [ 2; 1; 2; 1; 0 ]
              [ 3; 0; 2; 1; 0 ]
              [ 3; 1; 2; 1; 0 ] ]

        let actual = Universe.filter_rows (state, [ 3 ])
        Assert.AreEqual(expected, actual)

        let expected =
            [ [ 0; 1; 2; 1; 0 ]; [ 1; 1; 2; 1; 0 ]; [ 2; 1; 2; 1; 0 ]; [ 3; 1; 2; 1; 0 ] ]

        let actual = Universe.filter_rows (state, [ 3; 1 ])
        Assert.AreEqual(expected, actual)

        let actual = Universe.filter_rows (state, [ 1; 3 ])
        Assert.AreEqual(expected, actual)

        let actual = Universe.filter_rows (expected, [ 3; 1 ])
        Assert.AreEqual(expected, actual)

        let expected = [ [ 1; 1; 2; 1; 0 ]; [ 2; 1; 2; 1; 0 ]; [ 3; 1; 2; 1; 0 ] ]

        let actual = Universe.filter_rows (state, [ 0; 3; 1 ])
        Assert.AreEqual(expected, actual)

        let expected: QuantumState = []
        let actual = Universe.filter_rows (state, [ 0; 3; 1; 2; 4 ])
        Assert.AreEqual(expected, actual)

        let actual = Universe.filter_rows (state, [ 4 ])
        Assert.AreEqual(expected, actual)


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

          [ a.Where(LessThanEquals, b); b ],
          [ [ 0; 0 ]; [ 0; 1 ]; [ 0; 2 ]; [ 0; 3 ]; [ 1; 1 ]; [ 1; 2 ]; [ 1; 3 ]; [ 2; 2 ]; [ 2; 3 ]; [ 3; 3 ] ]

          [ a; b; c ],
          [ [ 0; 0; 0 ]
            [ 0; 1; 1 ]
            //[ 0; 2; 2 ]
            //[ 0; 3; 3 ]
            [ 1; 0; 1 ]
            [ 1; 1; 2 ]
            //[ 1; 2; 3 ]
            [ 1; 3; 0 ]
            // [ 2; 0; 2 ]
            // [ 2; 1; 3 ]
            [ 2; 2; 0 ]
            [ 2; 3; 1 ]
            //[ 3; 0; 3 ]
            [ 3; 1; 0 ]
            [ 3; 2; 1 ]
            //[ 3; 3; 2 ]
            ]

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

        printfn "outputs: %A\nfilters: %A\nmemory: %A\n" actual.Outputs actual.Filters actual.State
        Assert.AreEqual(expected.State, actual.State)
        Assert.AreEqual(expected.Outputs, actual.Outputs)
        Assert.AreEqual(expected.Filters, actual.Filters)
