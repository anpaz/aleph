namespace aleph.qsharp.grover {

    open Microsoft.Quantum.Diagnostics;
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Measurement;
    open Microsoft.Quantum.Convert;

    open aleph.qsharp.log as log;

    operation Apply(oracle: (Qubit[], Qubit) => Unit is Adj, all: Qubit[], output: Qubit[], answers: Int) : Unit 
    is Adj {
        let n = Length(output);
        let iterations = GroverIterationsCount(n, answers);
        log.Info($"Starting {iterations} iterations for output:{output} with {answers} answers (total qubits: {Length(all)})");

        for i in 1..iterations {
            GroverIteration(_toPhaseOracle(oracle, _), all, output, log.DEBUG_ON() and i == 1);
        }
    }

    function GroverIterationsCount(n: Int, answers: Int) : Int {
        let domain = (1 <<< n);
        let k = Max([1, answers]);
        let iterations = Floor(PI() * Sqrt(IntAsDouble(domain / k)) / 4.0);
        log.Debug($"Grover. iterations:{iterations} (n: {n}, a: {answers})");
        return Max([1,iterations]);
    }

    operation GroverOperator (output: Qubit[]) : Unit is Adj {
        let n = Length(output);
        within {
            ApplyToEachA(H, output);
            ApplyToEachA(X, output);
        }
        apply {
            Controlled Z(output[1..n-1], output[0]);
        }
    }
    
    operation GroverIteration (oracle : (Qubit[] => Unit is Adj), all: Qubit[], output: Qubit[], debug: Bool) : Unit
    is Adj {
        if (debug) {
            Message($"[Q#] -- Before oracle --");
            DumpRegister((), output);
            Message("");
        }

        oracle(all);

        if (debug) {
            Message($"[Q#] -- After oracle --");
            DumpRegister((), output);
            Message("");
        }

        GroverOperator(output);

        if (debug) {
            Message($"[Q#] -- After grover operator --");
            DumpRegister((), output);
            Message("");
        }
    }
    
    operation _toPhaseOracle(oracle : (Qubit[], Qubit) => Unit is Adj, register: Qubit[]) : Unit 
    is Adj {
        use target = Qubit() {
            within {
                X(target);
                H(target);
            }
            apply {
                oracle(register, target); 
            }
        }
    }
}
