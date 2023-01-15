namespace aleph.qsharp.grover {

    open Microsoft.Quantum.Random;
    open Microsoft.Quantum.Diagnostics;
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Measurement;
    open Microsoft.Quantum.Convert;

    open aleph.qsharp.log as log;

    operation Apply(oracle: (Qubit[], Qubit) => Unit is Adj, all: Qubit[], literals: Qubit[]) : Unit 
    {
        // Since we don't require the number of answers, we run grover multiple times, each with different
        // number of iterations, based on the algorithm in:
        // Tight Bounds on Quantum Searching (Boyer, Brassard, Hoyer, Tapp)
        use answer = Qubit();
        let n = Length(literals); // number of qubits
        let max = PI() * Sqrt(IntAsDouble(1 <<< n)) / 4.0; // max number of grover iterations
        mutable stop = Floor(max) * 2;  // we will eventually stop trying to find an answer (as none might exist)
        mutable m = 1.0;    // the max number of iteratiosn to try at each round.

        repeat {
            let iterations = DrawRandomInt(0, Floor(m));

            log.Info($"Starting {iterations} iterations for m:{m}, n:{n}, to-go:{stop}, max:{max}, literals:{literals}");
            for i in 1..iterations {
                GroverIteration(_toPhaseOracle(oracle, _), all, literals, log.DEBUG_ON() and i == 1);
            }

            oracle (all, answer);
        } until (M(answer) == One or stop < 0)
        fixup {
            set m = m * (8.0 / 7.0);
            set m = m > max ? max | m;
            set stop -= 1;

            ResetAll(literals);
            ApplyToEach(H, literals);

            Reset(answer);
        }
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
