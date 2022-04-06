namespace tuples {

    open Microsoft.Quantum.Measurement;
    open Microsoft.Quantum.Diagnostics;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Intrinsic;
    


    @EntryPoint()
    operation HelloQ() : Unit {
        use q2 = Qubit[1];
        use q1 = Qubit[2];
        use tracker = Qubit();

        let k1 = ket.Init_1([1, 3, 4], tracker, q1);
        ket.Print(k1);


        let tuples = [
            (0,1),
            (1,0),
            (2,0),
            //(2,1),
            (3,0)
        ];
        ResetAll(q1 + q2 + [tracker]);
        let k2 = ket.Init_2(tuples, tracker, (q1, q2));
        ket.Print(k2);

        let k3 = ket.Solve(k2);
        ket.Print(k3);


        // let k2 = ket.Solve(k1);
        // ket.Print(k2);

        // let v = M(tracker);
        // Message($"Measured: {v}");
        // if (v == Zero) {
        //     fail "Invalid measurement";
        // }

        // let ket = (tracker, q1 + q2);
        // DumpMachine();

        Message("Hello quantum world!");
        ResetAll(q1 + q2 + [tracker]);
    }
}

