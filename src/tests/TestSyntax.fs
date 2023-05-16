namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.quals.parser.ast

[<TestClass>]
type TestSyntax() =

    [<TestMethod>]
    member this.TestRawSyntax() =
        let plus = (Add 4)
        let times = (Multiply 8)

        let a = Ket(Literal 3)
        let b = Ket(Literal 3)

        Assert.AreEqual(a, a)
        Assert.AreSame(a, a)

        Assert.AreNotSame(a, b)
        Assert.AreNotEqual(a, b)

        let iszero = Ket(Map(IsZero, [ a ]))
        let equals = Ket(Map(Equals, [ a; iszero ]))
        let add = Ket(Map(plus, [ a; b ]))
        let addi = Ket(Map(times, [ a; Ket(Constant 3) ]))
        let color = Ket(Where(a, LessThan, [ Ket(Constant 3) ]))
        let eq = Ket(Where(b, Equals, [ a ]))
        let q = Ket(Map(If 3, [ color; add; addi ]))
        let values = Ket(Where(Ket(Literal 3), In [ 0; 2; 4; 6 ], []))
        let zero = Ket(Where(values, IsZero, []))

        Assert.AreSame(a, a)

    [<TestMethod>]
    member this.TestSyntacticSugar() =
        let a = Ket(Literal 2)
        let b = Ket(Literal 3)

        Assert.AreEqual(2, a.Add(0).Width)
        Assert.AreEqual(2, a.Add(1).Width)
        Assert.AreEqual(3, a.Add(b).Width)
        Assert.AreEqual(4, a.Add(12).Width)

        Assert.AreEqual(3, a.Multiply(b).Width)
        Assert.AreEqual(4, a.Multiply(12).Width)
        Assert.AreEqual(2, a.Multiply(1).Width)

        let f = a.Equals(b)
        let g = a.Equals(3)
        let h = a.Equals(false)
        Assert.AreEqual(1, f.Width)
        Assert.AreEqual(1, g.Width)
        Assert.AreEqual(1, h.Width)

        Assert.AreEqual(1, f.And(g).Width)
        Assert.AreEqual(1, f.Or(g).Width)
        Assert.AreEqual(1, f.Equals(g).Equals(false).Not().Width)

    [<TestMethod>]
    member this.TestCollectFilterIds() =
        let a = Ket(Literal 2)
        Assert.IsTrue(a.FilterId.IsNone)

        let b = Ket(Literal 3).Where(Equals, a)
        Assert.IsTrue(b.FilterId.IsSome)

        let c = a.Where(LessThan, Ket(Constant 3))
        Assert.IsTrue(c.FilterId.IsSome)
        Assert.AreNotEqual(b.FilterId.Value, c.FilterId.Value)
        Assert.AreEqual([c.FilterId.Value], Ket.CollectFilterIds [a; c])
        Assert.AreEqual([ b.FilterId.Value; c.FilterId.Value ], Ket.CollectFilterIds [a; b; c])

        let d = b.Equals(c)
        Assert.AreEqual([ b.FilterId.Value; c.FilterId.Value ], Ket.CollectFilterIds [d])

        let e = a.Add(c)
        Assert.AreEqual([ c.FilterId.Value ], Ket.CollectFilterIds [e])

        let f = b.Add(c)
        Assert.AreEqual([ b.FilterId.Value; c.FilterId.Value ], Ket.CollectFilterIds [f])

        // Make sure the filter ids appear only once:
        let e = a.Add(c).Where(LessThan, b).Where(Equals, c).Where(Or, b.Equals(c))
        let ids = Ket.CollectFilterIds [ e ]
        printf "%A" ids
        Assert.AreEqual(5, ids.Length)
        Assert.AreEqual(1, ids |> List.filter ((=) c.FilterId.Value) |> List.length)
        Assert.AreEqual(1, ids |> List.filter ((=) b.FilterId.Value) |> List.length)

        Assert.AreNotEqual(a, c)
        Assert.AreNotEqual(a, b)
