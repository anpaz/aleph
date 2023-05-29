namespace sandbox {
    open Microsoft.Quantum.Intrinsic;
    
    open aleph.qsharp.ket as ket;
    open aleph.qsharp.universe;
    open aleph.qsharp.value;

    @EntryPoint()
    operation Main() : Unit {
        Message("Hello quantum world!");

        let u1 = BigBang();

        // let (u2, r2) = ket.All(1, u1);
        // let (u3, r3) = ket.All(1, u2);
        // let (u4, r4) = ket.Add(r2[0], r3[0], 2, u3);
        // let (u5, r5) = ket.Constant(Value(1,1), u4);
        // let (u6, r6) = ket.LessThanEqual(r4[0], r5[0], u5);
        // let (u7) = ket.Filter(r6[0], u6);

        // Print(u6);
        // let final = Sample(u7, r2 + r3 + r4 + r5 + r6);

        let (u2, r2) = ket.All(2, u1);
        let (u3, r3) = ket.InSet([], r2[0], u2);
        let (u4) = ket.Filter(r3[0], u3);

        Print(u3);
        let final = Sample(u4, r2 + r3);

        Message($"result: {final}");
    }
}

