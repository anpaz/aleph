namespace tuples {

    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Characterization;
    open Microsoft.Quantum.Measurement;
    open Microsoft.Quantum.Diagnostics;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Intrinsic;
    

    @EntryPoint()
    operation HelloQ() : Unit {
        let k1 = ket.Init_1([0, 1, 2]);
        //ket.Print(k1);

        let tuples = [
            (0,0),
            (0,2),
            (1,3),
            (1,2),
            (2,3),
            (2,2),
            (3,0),
            (3,3)
        ];
        let k2 = ket.Init_2(tuples);
        ket.Print(k2);

        let k3 = ket.Solve(k2);
        ket.Print(k3);

        Message("=. aleph poc .=");
    }
}

