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
        // let k1 = ket.Tuple_1([0, 1, 2]);
        // ket.Print(k1);

        let tuples = [
            (0,0),
            // (2,0),
            (2,1),
            // (1,2),
            (2,2),
            (0,3),
            // (1,3),
            // (2,3),
            (2,3)
        ];
        let k2 = ket.Tuple_2(tuples);
        ket.Print(k2);

        //ket.Print(ket.Project(k2, [0]));
        //ket.Print(ket.Project(k2, [1]));
        // ket.Print(ket.Add(k2));

        let k3 = ket.Apply(k2, Addition);
        // ket.Print(k3);

        let k4 = ket.Solve(k3);
        ket.Print(k4);

        let a1 = ket.Sample(k4);
        Message($"a: {a1}");

        // let a2 = ket.Sample(k4);
        // Message($"a: {a2}");

        // let a3 = ket.Sample(k4);
        // Message($"a: {a3}");

        Sign();
    }

    operation Addition(x: Qubit[], y: Qubit[], answer: Qubit[]) : Unit
    is Adj
    {
        //TODO: let l = Length(x);
        CNOT(x[0], answer[0]);
        CNOT(y[0], answer[0]);
        CCNOT(x[0], y[0], answer[1]);

        CNOT(x[1], answer[1]);
        CNOT(y[1], answer[1]);
    }

    function Sign() : Unit {
        Message("");
        Message("=. aleph (poc) .=");
        Message("");
    }
}

