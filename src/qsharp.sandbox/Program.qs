namespace sandbox {
    open Microsoft.Quantum.Intrinsic;
    
    open aleph.qsharp.ket as ket;
    open aleph.qsharp.universe;
    open aleph.qsharp.value;
    open aleph.qsharp.register;

    function append<'T>(item: 'T, array: 'T[]) : 'T[] {
        return array + [item];
    }

    function literal(start: Int, size: Int) : (Register, Int) {
        return (NewLiteral(start, size), start + size);
    }

    function output(start: Int, size: Int) : (Register, Int) {
        return (NewOutput(start, size), start + size);
    }

    @EntryPoint()
    operation Main() : Unit {
        Message("Hello quantum world!");

        let n0 = 0;
        let m0 = [];
        let o0 = [];

        // let x = ket 3
        let (r1, n1) = literal(n0, 3); 

        // 1
        let (r2, n2) = output(n1, 1);
        let m1 = append(ket.Constant(Value(1, 1), r2), m0);

        // let y = ket 3 
        let (r3, n3) = literal(n2, 3);
        
        // y <= 1
        let (r4, n4) = output(n3, 1);
        let m2 = append(ket.LessThanEqual(r3, r2, r4), m1);

        // where y <= 1
        //let o1 = append(ket.Filter(r4), o0);

        // 3
        let (r5, n5) = output(n4, 3);
        let m3 = append(ket.Constant(Value(3, 3), r5), m2);

        // x <= 3
        let (r6, n6) = output(n5, 1);
        let m4 = append(ket.LessThanEqual(r1, r5, r6), m3);
        // let m5 = append(ket.If(r6, ket.Copy(r1, r7), ket.Add(r1, r3, r7)), m4);

        // x + y
        let (r7, n7) = output(n6, 4);
        let m5 = append(ket.Add(r1, r3, r7), m4);

        // if x <= 2 then x else x + y
        let (r8, n8) = output(n7, 4);
        let m6 = append(ket.If(r6, r1, r7, r8), m5);

        let u = UniverseInfo(n8, [r1, r2, r3, r4, r5, r6, r7, r8], m6, o0);
        
        //Print(u);
        for i in 1..15 {
            let final = Sample(u, [r1, r3, r6]);
            Message($"result: {final}");
        }
    }
}
