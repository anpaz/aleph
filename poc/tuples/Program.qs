namespace tuples {

    open Microsoft.Quantum.Measurement;
    open Microsoft.Quantum.Diagnostics;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Intrinsic;
    


    @EntryPoint()
    operation HelloQ() : Unit {
        use q1 = Qubit[2];
        use q2 = Qubit[1];
        use t1 = Qubit();

        let k1 = ket.Init_1([0, 1, 2], t1, q1);
        //ket.Print(k1);

        let tuples = [
            (0,0),
            (1,0),
            (2,0),
            (2,1),
            (3,0)
        ];
        let k2 = ket.Init_2(tuples, t1, (q1, q2));
        ket.Print(k2);

        use t2 = Qubit();
        let k3 = ket.Solve(k2, t2);
        ket.Print(k3);

        Message("Hello quantum world!");
        ResetAll(q1 + q2 + [t1, t2]);
    }
}

