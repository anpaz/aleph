namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.kets
open aleph.qpu.qsharp

open aleph.quals.tests.Utils


[<TestClass>]
type TestQPUQsharp() =
    member this.QPU = Processor(context.simulator)


    [<TestMethod>]
    member this.TestBasicExpressions() =
        let a = ket 2
        let b = ket 2

        [ [ KetValue(Literal 1) ], [ [ 0 ]; [ 1 ] ]

          [ a ], [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ] ]

          [ b ], [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ] ]

          [ a; a ], [ [ 0; 0 ]; [ 1; 1 ]; [ 2; 2 ]; [ 3; 3 ] ]

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
          [ [ 0; 0 ]
            [ 0; 1 ]
            [ 0; 2 ]
            [ 1; 0 ]
            [ 1; 1 ]
            [ 1; 2 ]
            [ 2; 0 ]
            [ 2; 1 ]
            [ 2; 2 ]
            [ 3; 0 ]
            [ 3; 1 ]
            [ 3; 2 ] ]

          [ a; b.Where(Eq, 0) ], [ [ 0; 0 ]; [ 1; 0 ]; [ 2; 0 ]; [ 3; 0 ] ]


          [ a.Where(GreaterThan, 7); b ], []

          [ a; ket 1 ], [ [ 0; 0 ]; [ 0; 1 ]; [ 1; 0 ]; [ 1; 1 ]; [ 2; 0 ]; [ 2; 1 ]; [ 3; 0 ]; [ 3; 1 ] ]

          [ ket 1; KetValue(Constant 4) ], [ [ 0; 4 ]; [ 1; 4 ] ] ]
        |> List.iter (AssertSample this.QPU)

    [<TestMethod>]
    member this.TestAddMultiply() =
        let a = KetValue(Literal 1)
        let b = KetValue(Literal 1)

        let c =
            KetValue(Literal 2)
                .Multiply(3)
                .Where(LessThanEquals, KetValue(Constant 3))
                .Add(b, width = 4)

        let d =
            KetValue(Literal 2)
                .Multiply(3, width = 4)
                .Add(b)
                .Where(GreaterThan, KetValue(Constant 0))

        let e = c.Add(a.Where(Not))

        [ [ a; b; KetValue(Map(Add(1), [ a; b ])) ], [ [ 0; 0; 0 ]; [ 0; 1; 1 ]; [ 1; 0; 1 ]; [ 1; 1; 0 ] ]

          [ a; b; a.Add(b) ], [ [ 0; 0; 0 ]; [ 0; 1; 1 ]; [ 1; 0; 1 ]; [ 1; 1; 2 ] ]

          [ a; b; KetValue(Map(Add(2), [ a; b ])).Where(GreaterThan, 1) ], [ [ 1; 1; 2 ] ]

          [ b; c ], [ [ 0; 0 ]; [ 0; 3 ]; [ 1; 3 ]; [ 1; 4 ] ]

          [ b; d ], [ [ 0; 3 ]; [ 0; 6 ]; [ 0; 1 ]; [ 1; 1 ]; [ 1; 4 ]; [ 1; 7 ]; [ 1; 2 ] ]

          [ e ], [ [ 0 ]; [ 1 ]; [ 3 ]; [ 4 ]; [ 2 ] ] ]
        |> List.iter (AssertSample this.QPU)


    [<TestMethod>]
    member this.TestBoolean() =
        let a = KetValue(Literal 1)
        let b = KetValue(Literal 2)
        let c = a.Add(b).Where(GreaterThan, 1)

        // TODO: The compiler should know that
        //   d = a.LessThan(b)
        //   e = a.LessThan(b)
        // can point to the same ket.
        let d = a.LessThanEquals(b).And(b.LessThanEquals(1))
        let e = a.LessThanEquals(b).Or(b.LessThanEquals(1))
        let f = e.Not().Where(Eq, 1)

        [ [ a; b; a.LessThanEquals(b) ],
          [ [ 0; 0; 1 ]; [ 0; 1; 1 ]; [ 0; 2; 1 ]; [ 0; 3; 1 ]; [ 1; 0; 0 ]; [ 1; 1; 1 ]; [ 1; 2; 1 ]; [ 1; 3; 1 ] ]

          [ a; b; b.LessThanEquals(0) ],
          [ [ 0; 0; 1 ]; [ 0; 1; 0 ]; [ 0; 2; 0 ]; [ 0; 3; 0 ]; [ 1; 0; 1 ]; [ 1; 1; 1 ]; [ 1; 2; 0 ]; [ 1; 3; 0 ] ]


          [ c ], [ [ 2 ]; [ 3 ] ]

          [ d; e ], [ [ 1; 1 ]; [ 0; 1 ] ]

          [ f ], [] ]
        |> List.iter (AssertSample this.QPU)

    [<TestMethod>]
    member this.TestIn() =
        let a = KetValue(Literal 2)

        [ [ a; a.In [ 0; 2 ] ], [ [ 0; 1 ]; [ 1; 0 ]; [ 2; 1 ]; [ 3; 0 ] ]

          [ a; a.Where(In [ 0; 2 ]) ], [ [ 0; 0 ]; [ 2; 2 ] ]

          [ a; a.Where(In []); a.In [ 1; 2; 3; 4 ] ], []

          [ a; a.Where(In []); KetValue(Literal 2).In [ 1; 3 ] ], [] ]
        |> List.iter (AssertSample this.QPU)

    [<TestMethod>]
    member this.TestId() =
        let a = KetValue(Literal 2)

        [ [ a.Where(Id) ], [ [ 3 ] ]

          [ a; a.GreaterThan(2).Where(Id) ], [ [ 3; 1 ] ] ]
        |> List.iter (AssertSample this.QPU)


    [<TestMethod>]
    member this.TestChoose() =
        let a = KetValue(Literal 2)
        let b = a.Add(4, width = 3)

        let c = a.In([ 1; 2 ]).Choose(a, b)

        [ [ a; b; c ], [ [ 0; 4; 4 ]; [ 1; 5; 1 ]; [ 2; 6; 2 ]; [ 3; 7; 7 ] ] ]
        |> List.iter (AssertSample this.QPU)

    [<TestMethod>]
    member this.TestSample() =
        let a = KetValue(Literal 2)
        let b = KetValue(Literal 2)
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
        let a = KetValue(Literal 2)
        let b = KetValue(Literal 2)

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
