namespace aleph.qsharp {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Arrays;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Diagnostics;

    open aleph.qsharp.grover as grover;
    open aleph.qsharp.log as log;

    function BigBang(): Universe {
        return Universe(1, 1, [_tracker(_, _)]);
    }
    
    operation Sample(universe: Universe, register: Register[]) : Value[] {
        let (_, columns, oracle) = universe!;

        use qubits = Qubit[columns]; // One extra for tracker

        Prepare(universe, qubits);

        mutable result = [];
        for r in register {
            let value = ResultArrayAsInt(ForEach(M, qubits[r!]));
            let size = Length(RangeAsIntArray(r!));
            set result += [ Value(value, size) ];
        }

        ResetAll(qubits);
        return result;
    }

    operation Prepare(universe: Universe, qubits: Qubit[]) : Unit {
        let (rows, _, _) = universe!;
        let oracle = _oracle(universe, _, _);
        let tracker = qubits[0];
        mutable max = 2;

        repeat {
            ResetAll(qubits);
            ApplyToEachA(H, qubits);
            grover.Apply(oracle, qubits, rows);
        } until ((M(tracker) == One) or (rows == 0) or (max <= 0))
        fixup {
            set max = max - 1;
        }

        // if (M(tracker) == Zero) {
        //     fail "Fail to prepare Universe.";
        // }

        if (log.DEBUG_ON()) {
            Message($"[Q#] Final state after Prepare: ");
            if (Length(qubits) > 1) {
                DumpRegister((), qubits[1..Length(qubits)-1]);
            }
            Message("");
        }
    }
    
    operation _tracker (qubits: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($".tracker.");
        CNOT(qubits[0], target);
    }

    operation _oracle(universe: Universe, all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        let (_, _, oracles) = universe!;

        let n = Length(oracles);
        use ancillas = Qubit[n];

        within {
            for i in 0..n-1 {
                let o = oracles[i];
                let a = ancillas[i];
                o(all, a);
            }
        } apply {
            Controlled X (ancillas, target);
        }
    }
}