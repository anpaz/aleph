namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open System
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
        let a = ket 2
        let b = ket 2

        [ [ ket 1 ], [ [ 0 ]; [ 1 ] ]

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

          [ a; ket 1 ], [ [ 0; 0 ]; [ 0; 1 ]; [ 1; 0 ]; [ 1; 1 ]; [ 2; 0 ]; [ 2; 1 ]; [ 3; 0 ]; [ 3; 1 ] ]

          [ ket 1; constant 4 ], [ [ 0; 4 ]; [ 1; 4 ] ] ]
        |> List.iter (this.TestExpression)

    [<TestMethod>]
    member this.TestAddMultiply() =
        let a = ket 1
        let b = ket 1

        let c = (ket 2).Multiply(3).Where(LessThanEquals, constant 3).Add(b, width = 4)

        let d = (ket 2).Multiply(3, width = 3).Add(b).Where(GreaterThan, constant 0)

        let e = c.Add(a.Where(Not))

        [ [ map (Add 1) [ a; b ] ], [ [ 0; 0; 0 ]; [ 0; 1; 1 ]; [ 1; 0; 1 ]; [ 1; 1; 0 ] ]

          [ a.Add(b) ], [ [ 0; 0; 0 ]; [ 0; 1; 1 ]; [ 1; 0; 1 ]; [ 1; 1; 2 ] ]

          [ map (Add 2) [ a; b ] ], [ [ 0; 0; 0 ]; [ 0; 1; 1 ]; [ 1; 0; 1 ]; [ 1; 1; 2 ] ]

          [ c ], [ [ 0; 3; 0; 3; 1; 0; 0 ]; [ 0; 3; 0; 3; 1; 1; 1 ]; [ 1; 3; 3; 3; 1; 0; 3 ]; [ 1; 3; 3; 3; 1; 1; 4 ] ]

          [ d ],
          [ [ 0; 3; 0; 1; 1; 0; 1 ]
            [ 1; 3; 3; 0; 3; 0; 1 ]
            [ 1; 3; 3; 1; 4; 0; 1 ]
            [ 2; 3; 6; 0; 6; 0; 1 ]
            [ 2; 3; 6; 1; 7; 0; 1 ]
            [ 3; 3; 1; 0; 1; 0; 1 ]
            [ 3; 3; 1; 1; 2; 0; 1 ] ]

          [ e ],
          [ [ 0; 3; 0; 3; 1; 0; 0; 0; 1; 0 ]
            [ 0; 3; 0; 3; 1; 1; 1; 0; 1; 1 ]
            [ 1; 3; 3; 3; 1; 0; 3; 0; 1; 3 ]
            [ 1; 3; 3; 3; 1; 1; 4; 0; 1; 4 ] ] ]
        |> List.iter (this.TestExpression)


    [<TestMethod>]
    member this.TestBoolean() =
        let a = ket 1
        let b = ket 1
        let c = a.Add(b, width = 2).Where(LessThanEquals, 1)

        // TODO: The compiler should know that
        //   d = a.LessThan(b)
        //   e = a.LessThan(b)
        // can point to the same ket.
        let d = a.LessThanEquals(b).And(b.LessThanEquals(1))
        let e = a.LessThanEquals(b).Or(b.LessThanEquals(1))
        let f = e.Not().Where(Eq, 1)

        [ [ a.LessThanEquals(b) ], [ [ 0; 0; 1 ]; [ 0; 1; 1 ]; [ 1; 0; 0 ]; [ 1; 1; 1 ] ]

          [ b.LessThanEquals(0) ], [ [ 0; 0; 1 ]; [ 1; 0; 0 ] ]

          [ c ], [ [ 0; 0; 0; 1; 1 ]; [ 0; 1; 1; 1; 1 ]; [ 1; 0; 1; 1; 1 ] ]

          [ d; e ],
          [ [ 0; 0; 1; 1; 1; 1; 1; 1; 1; 1 ]
            [ 0; 1; 1; 1; 1; 1; 1; 1; 1; 1 ]
            [ 1; 0; 0; 1; 1; 0; 0; 1; 1; 1 ]
            [ 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 ] ]

          [ f ], [ [ -1; -1; -1; -1; -1; -1; -1; -1; -1 ] ] ]
        |> List.iter (this.TestExpression)

    [<TestMethod>]
    member this.TestIn() =
        let a = ket 2

        [ [ a.In [ 0; 2 ] ], [ [ 0; 1 ]; [ 1; 0 ]; [ 2; 1 ]; [ 3; 0 ] ]

          [ a.Where(In [ 0; 2 ]) ], [ [ 0; 1 ]; [ 2; 1 ] ]

          [ a; a.Where(In []); a.In [ 1; 2; 3; 4 ] ], [ [ -1; -1; 0 ] ]

          [ a; a.Where(In []); (ket 2).In [ 1; 3 ] ],
          [ [ -1; -1; 0; 0 ]; [ -1; -1; 1; 1 ]; [ -1; -1; 2; 0 ]; [ -1; -1; 3; 1 ] ] ]
        |> List.iter (this.TestExpression)


    [<TestMethod>]
    member this.TestId() =
        let a = ket 2

        [ [ a.Where(Id) ], [ [ 1 ]; [ 2 ]; [ 3 ] ]

          [ a; a.GreaterThan(2).Where(Id) ], [ [ 3; 2; 1 ] ] ]
        |> List.iter (this.TestExpression)


    [<TestMethod>]
    member this.TestChoose() =
        let a = ket 2
        let b = a.Add(4, width = 3)

        let c = a.In([ 1; 2 ]).Choose(a, b)

        [ [ c ], [ [ 0; 0; 4; 4; 4 ]; [ 1; 1; 4; 5; 1 ]; [ 2; 1; 4; 6; 2 ]; [ 3; 0; 4; 7; 7 ] ] ]
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
            |> QuantumState

        let expected =
            [ [ 0; 0; 2; 1; 0 ]
              [ 0; 1; 2; 1; 0 ]
              [ 1; 0; 2; 1; 0 ]
              [ 1; 1; 2; 1; 0 ]
              [ 2; 0; 2; 1; 0 ]
              [ 2; 1; 2; 1; 0 ]
              [ 3; 0; 2; 1; 0 ]
              [ 3; 1; 2; 1; 0 ] ]
            |> QuantumState

        let actual = state.FilterRows 3
        Assert.AreEqual(expected.Rows, actual.Rows)

        let expected =
            [ [ 0; 1; 2; 1; 0 ]; [ 1; 1; 2; 1; 0 ]; [ 2; 1; 2; 1; 0 ]; [ 3; 1; 2; 1; 0 ] ]
            |> QuantumState

        let actual = (state.FilterRows 3).FilterRows 1
        Assert.AreEqual(expected.Rows, actual.Rows)

        let actual = (state.FilterRows 1).FilterRows 3
        Assert.AreEqual(expected.Rows, actual.Rows)

        let actual = (expected.FilterRows 3).FilterRows 1
        Assert.AreEqual(expected.Rows, actual.Rows)

        let expected =
            [ [ 1; 1; 2; 1; 0 ]; [ 2; 1; 2; 1; 0 ]; [ 3; 1; 2; 1; 0 ] ] |> QuantumState

        let actual = ((state.FilterRows 0).FilterRows 3).FilterRows 1
        Assert.AreEqual(expected.Rows, actual.Rows)

        let expected = [ [ -1; -1; -1; -1; -1 ] ] |> QuantumState

        let actual =
            ((((state.FilterRows 0).FilterRows 3).FilterRows 1).FilterRows 2).FilterRows 4

        Assert.AreEqual(expected.Rows, actual.Rows)

        let actual = (state.FilterRows 4)
        Assert.AreEqual(expected.Rows, actual.Rows)


    [<TestMethod>]
    member this.TestSample() =
        let a = ket 2
        let b = ket 2
        let c = a.Add(b).Where(LessThanEquals, 1)

        // TODO: The compiler should know that
        //   d = a.LessThan(b)
        //   e = a.LessThan(b)
        // can point to the same ket.
        let d = a.LessThanEquals(b).And(b.LessThanEquals(1))
        let e = a.LessThanEquals(b).Or(b.LessThanEquals(1))
        let f = e.Not().Where(Eq, 1)

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


    [<TestMethod>]
    member this.TestSampleWhen() =
        let a = ket 2
        let b = ket 2

        [ [ a ], None, [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ] ]

          [ b ], b.LessThanEquals(2) |> Some, [ [ 0 ]; [ 1 ]; [ 2 ] ]

          [ a; b ],
          b.LessThanEquals(1) |> Some,
          [ [ 0; 0 ]; [ 0; 1 ]; [ 1; 0 ]; [ 1; 1 ]; [ 2; 0 ]; [ 2; 1 ]; [ 3; 0 ]; [ 3; 1 ] ]

          [ a; b ],
          a.LessThanEquals(b) |> Some,
          [ [ 0; 0 ]; [ 0; 1 ]; [ 0; 2 ]; [ 0; 3 ]; [ 1; 1 ]; [ 1; 2 ]; [ 1; 3 ]; [ 2; 2 ]; [ 2; 3 ]; [ 3; 3 ] ]

          [ a; b ],
          a.LessThanEquals(b) |> Some,
          [ [ 0; 0 ]; [ 0; 1 ]; [ 0; 2 ]; [ 0; 3 ]; [ 1; 1 ]; [ 1; 2 ]; [ 1; 3 ]; [ 2; 2 ]; [ 2; 3 ]; [ 3; 3 ] ]

          [ a; b ],
          a.GreaterThan(1) |> Some,
          [ [ 2; 0 ]; [ 2; 1 ]; [ 2; 2 ]; [ 2; 3 ]; [ 3; 0 ]; [ 3; 1 ]; [ 3; 2 ]; [ 3; 3 ] ]

          [ a; b ], a.GreaterThan(1).And(b.GreaterThan(a)) |> Some, [ [ 2; 3 ] ] ]
        |> List.iter (AssertSampleWithFilter this.QPU)

    [<TestMethod>]
    member this.TestHistogram() =

        let k1 = ket 2
        let k2 = ket 2
        let k3 = k1.Add(k2, width = 3)

        [ [ k3 ], [ ([ 0 ], 1); ([ 1 ], 2); ([ 2 ], 3); ([ 3 ], 4); ([ 4 ], 3); ([ 5 ], 2); ([ 6 ], 1) ]

          [ k1.Where(LessThanEquals, 2); k3.Where(GreaterThan, 2) ],
          [ ([ 0; 3 ], 1); ([ 1; 3 ], 1); ([ 1; 4 ], 1); ([ 2; 3 ], 1); ([ 2; 4 ], 1); ([ 2; 5 ], 1) ] ]
        |> List.iter this.TestHistogram

    member this.TestHistogram(kets, expected) =
        let ctx = { qpu = this.QPU }
        let rounds = 10000

        let histogram (u: IUniverse) =
            match u.Histogram(kets, rounds) with
            | Ok value -> value
            | Error msg -> failwith "Error when getting the histogram"

        let to_percent (table: list<int list * int>) =
            let total = table |> List.sumBy (fun (idx, value) -> value)
            table |> List.map (fun (idx, value) -> (idx, value * 100 / total))

        match prepare ctx kets with
        | Ok actual ->
            let actual = histogram actual |> Map.toSeq |> Seq.sort |> Seq.toList
            printfn "original expected: %A\n" expected
            printfn "original actual: %A\n" actual

            // Check first that the number of values match the requested rounds
            let total = actual |> List.sumBy (fun (_, value) -> value)
            Assert.AreEqual(rounds, total)

            // Check that each value is within 2% of the expected value.
            // To do this we convert the expected/actual tables such that each item
            // value is the %, not the actual value
            let check_one ((k1, v1: int), (k2, v2: int)) =
                Assert.AreEqual(k1, k2)
                Assert.IsTrue(Math.Abs(v1 - v2) < 3)

            let expected = expected |> List.sort |> to_percent
            let actual = actual |> to_percent
            printfn "percent expected: %A\n" expected
            printfn "percent actual: %A\n" actual
            List.zip expected actual |> List.iter check_one
        | Error msg -> Assert.Fail msg

    member this.TestExpression(exprs, expected) =
        printfn "expr: %A" exprs

        let ctx = { qpu = this.QPU }

        match prepare ctx exprs with
        | Ok actual ->
            let actual = actual :?> Universe
            printfn "state: %A\n" actual.State.Rows
            Assert.AreEqual(expected, actual.State.Rows)
        | Error msg -> Assert.Fail msg
