namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.kets

[<TestClass>]
type TestSyntax() =

    [<TestMethod>]
    member this.TestRawSyntax() =
        let plus = (Add 4)
        let times = (Multiply 8)

        let a = KetValue(Literal 3)
        let b = KetValue(Literal 3)

        Assert.AreEqual(a, a)
        Assert.AreSame(a, a)

        Assert.AreNotSame(a, b)
        Assert.AreNotEqual(a, b)

        let iszero = KetValue(Map(Not, [ a ]))
        let equals = KetValue(Map(Eq, [ a; iszero ]))
        let add = KetValue(Map(plus, [ a; b ]))
        let addi = KetValue(Map(times, [ a; KetValue(Constant 3) ]))
        let color = KetValue(Where(a, LessThanEquals, [ KetValue(Constant 3) ]))
        let eq = KetValue(Where(b, Eq, [ a ]))
        let q = KetValue(Map(If, [ color; add; addi ]))
        let values = KetValue(Where(KetValue(Literal 3), In [ 0; 2; 4; 6 ], []))
        let zero = KetValue(Where(values, Not, []))

        Assert.AreSame(a, a)

    [<TestMethod>]
    member this.TestSyntacticSugar() =
        let a = KetValue(Literal 2)
        let b = KetValue(Literal 3)

        Assert.AreEqual(3, a.Add(0).Width)
        Assert.AreEqual(3, a.Add(1).Width)
        Assert.AreEqual(4, a.Add(b).Width)
        Assert.AreEqual(5, a.Add(12).Width)

        Assert.AreEqual(5, a.Multiply(b).Width)
        Assert.AreEqual(6, a.Multiply(12).Width)
        Assert.AreEqual(3, a.Multiply(1).Width)
        Assert.AreEqual(3, a.Multiply(0).Width)

        let f = a.Equals(b)
        let g = a.Equals(3)
        let h = a.Equals(false)
        Assert.AreEqual(1, f.Width)
        Assert.AreEqual(1, g.Width)
        Assert.AreEqual(1, h.Width)

        Assert.AreEqual(1, f.And(g).Width)
        Assert.AreEqual(1, f.Or(g).Width)
        Assert.AreEqual(3, g.Choose(a, b).Width)
        Assert.AreEqual(1, f.Equals(g).Equals(false).Not().Width)
