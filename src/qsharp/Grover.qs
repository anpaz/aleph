namespace aleph.qsharp.grover {

    open Microsoft.Quantum.Diagnostics;
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Measurement;
    open Microsoft.Quantum.Convert;

    open aleph.qsharp.log as log;

    operation Apply(oracle: (Qubit[], Qubit) => Unit is Adj, register : Qubit[], answers: Int) : Unit 
    is Adj {
        let n = Length(register);

        let iterations = GroverIterationsCount(n, answers);
        log.Info($"Starting {iterations} iterations for {n} qubits with {answers} answers");

        for i in 1..iterations {
            GroverIteration(_toPhaseOracle(oracle, _), register);
            if (log.DEBUG_ON()) {
                Message($"[Q#] -- Grover iteration {i} --");
                DumpRegister((), register);
                Message("");
            }
        }
    }

    function GroverIterationsCount(n: Int, answers: Int) : Int {
        let domain = (1 <<< n);
        let k = Max([1, answers]);
        let iterations = Floor(PI() * Sqrt(IntAsDouble(domain / k)) / 4.0);
        log.Debug($"Grover. iterations:{iterations} (n: {n}, a: {answers})");
        return Max([1,iterations]);
    }

    operation GroverOperator (register : Qubit[]) : Unit is Adj {
        // ...
        let n = Length(register);
        within {
            ApplyToEachA(H, register);
            ApplyToEachA(X, register);
        } apply {
            Controlled Z(register[1..n-1], register[0]);
        }
    }
    
    operation GroverIteration (oracle : (Qubit[] => Unit is Adj), register : Qubit[]) : Unit 
    is Adj {
        oracle(register);
        GroverOperator(register);
    }
    
    operation _toPhaseOracle(oracle : (Qubit[], Qubit) => Unit is Adj, register: Qubit[]) : Unit 
    is Adj {
        use target = Qubit() {
            within {
                X(target);
                H(target);
            } apply {
                oracle(register, target);
            }
        }
    }
}
