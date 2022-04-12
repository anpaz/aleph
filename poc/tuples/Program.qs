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
        let k1 = ket.Tuple_1([0, 1, 2]);
        ket.Print(k1);

        let tuples = [
            (0,3),
            (0,2),
            // (1,3),
            (1,2),
            // (2,3),
            // (2,2),
            (2,3),
            (2,1)
        ];
        let k2 = ket.Tuple_2(tuples);
        //ket.Print(k2);

        let k3 = ket.Solve(k2);
        //ket.Print(k3);

        let k4 = ket.Apply(k2, Add);
        //ket.Print(k4);

        let k5 = ket.Solve(k4);
        //ket.Print(k5);

        // let a1 = ket.Sample(k5);
        // Message($"a: {a1}");

        // let a2 = ket.Sample(k5);
        // Message($"a: {a2}");

        // let a3 = ket.Sample(k5);
        // Message($"a: {a3}");

        Message("");
        Message("=. aleph (poc) .=");
        Message("");
    }

    operation Add(x: Qubit[], y: Qubit[], answer: Qubit[]) : Unit
    is Adj
    {
        //TODO: let l = Length(x);
        CNOT(x[0], answer[0]);
        CNOT(y[0], answer[0]);
        CCNOT(x[0], y[0], answer[1]);

        CNOT(x[1], answer[1]);
        CNOT(y[1], answer[1]);
    }
}

