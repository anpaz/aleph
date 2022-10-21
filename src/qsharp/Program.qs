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

        // let (u2, r2) = ket.All(1, u1);
        // let (u3, r3) = ket.Tuples([
        //     [Value(0, 1), Value(1, 1)],
        //     [Value(1, 1), Value(1, 1)]
        // ], u2);
        // let (u4, r4) = ket.Constant(Value(1, 1), u3);
        // let (u5, r5) = ket.Equals(r3[0], r4[0], u4);
        // let u6 = ket.Filter(r4[0], 4, u5);

        let (u2, r2) = ket.All(2, u1);
        let (u3, r3) = ket.Tuples([
            [Value(2, 2)],
            [Value(1, 2)]], u2);
        //let (u3, r3) = ket.Constant(Value(2, 2), u2);
        let (u4, r4) = ket.Multiply(r2[0], r3[0], u3);
        // let (u4, r4) = ket.Not(r3[0], u3);
        // //let (u4, r4) = ket.Equals(r2, r3, u3);
        // //let (u4, r4) = ket.Add(r2, r3, u3);

        Print(u4);
    }
}

