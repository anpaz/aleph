namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.kets

[<TestClass>]
type TestSyntax() =

    [<TestMethod>]
    member this.TestRawSyntax() =
        let plus = (Add 4)
        let times = (Multiply 8)

        let a = ket 3
        let b = ket 3

        Assert.AreEqual(a, a)
        Assert.AreSame(a, a)

        Assert.AreNotSame(a, b)
        Assert.AreNotEqual(a, b)

        let iszero = map Not [ a ]
        Assert.AreEqual(typeof<KetValue>, iszero.GetType())

        let equals = map Eq [ a; iszero ]
        Assert.AreEqual(typeof<KetValue>, equals.GetType())

        let add = map plus [ a; b ]
        Assert.AreEqual(typeof<KetValue>, add.GetType())

        let addi = map times [ a; constant 3 ]
        Assert.AreEqual(typeof<KetValue>, addi.GetType())

        let color = where a LessThanEquals [ constant 3 ]
        Assert.AreEqual(typeof<KetValue>, color.GetType())

        let eq = where b Eq [ a ]
        Assert.AreEqual(typeof<KetValue>, eq.GetType())

        let q = map If [ color; add; addi ]
        Assert.AreEqual(typeof<KetValue>, q.GetType())

        let values = where (ket 3) (In [ 0; 2; 4; 6 ]) []
        Assert.AreEqual(typeof<KetValue>, values.GetType())

        let zero = where values Not []
        Assert.AreEqual(typeof<KetValue>, zero.GetType())

        Assert.AreSame(a, a)

    [<TestMethod>]
    member this.TestSyntacticSugar() =
        let a = ket 2
        let b = ket 3

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
