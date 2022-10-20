namespace aleph {
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Intrinsic;
    
    open aleph.qsharp.ket as ket;
    open aleph.qsharp.universe;
    open aleph.qsharp.value;

    @EntryPoint()
    operation HelloQ() : Unit {
        Message("Hello quantum world!");

        let u1 = BigBang();

        let (u2, r2) = ket.All(1, u1);
        let (u3, r3) = ket.All(1, u2);
        //let (u3, r3) = ket.Constant(Value(2, 2), u2);
        //let (u4, r4) = ket.And(r2, r3, u3);
        let (u4, r4) = ket.Not(r3, u3);
        //let (u4, r4) = ket.Equals(r2, r3, u3);
        //let (u4, r4) = ket.Add(r2, r3, u3);
        let u6 = ket.Filter(r4, 4, u4);

        Print(u6);
    }
}

