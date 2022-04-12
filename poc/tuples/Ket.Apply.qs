namespace ket {

    open Microsoft.Quantum.Intrinsic;


    operation Apply(k: Ket, f: (Qubit[], Qubit[], Qubit[])=> Unit is Adj) : Ket 
    {
        let (init, oldOracle, size, answers, oldRegisters, tracker, isValid) = k!;
        let r = Length(oldRegisters);


        let x = oldRegisters[r-2];
        let y = oldRegisters[r-1];
        let z = size..size+1;
        let registers = oldRegisters + [z];
        let oracle = _Apply_oracle(oldOracle, f, registers, _, _);

        log.Info($"Apply Init --> registers: {registers}");

        return Ket(init, oracle, size + 2, answers, registers, tracker, isValid);
    }

    operation _Apply_oracle(
        oracle: (Qubit[], Qubit) => Unit is Adj + Ctl,
        f: (Qubit[], Qubit[], Qubit[]) => Unit is Adj,
        registers: Range[],
        qubits: Qubit[],
        target: Qubit
        ): Unit 
    is Adj + Ctl {
        let x = qubits[registers[0]];
        let y = qubits[registers[1]];
        let z = qubits[registers[2]];
        
        use a = Qubit[2];
        use tracker = Qubit[2];
        
        within {
            oracle(qubits[0..Length(qubits)-3], tracker[0]);
            f(x, y, a);
            _areEqual(a, z, tracker[1]);
        } apply {
            Controlled X(tracker, target);
        }
    }


    operation _areEqual(x: Qubit[], y: Qubit[], answer: Qubit) : Unit
    is Adj + Ctl
    {
        let l = Length(x);
        use a = Qubit[l];

        within {
            for i in 0..l-1 {
                CNOT(x[i], a[i]);
                CNOT(y[i], a[i]);
                X(a[i]);
            }
        } apply {
            Controlled X (a, answer);
        }
    }

}