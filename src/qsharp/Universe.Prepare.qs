namespace aleph.qsharp.universe {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Arrays;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Diagnostics;

    open aleph.qsharp.grover as grover;
    open aleph.qsharp.ket as ket;
    open aleph.qsharp.log as log;
    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;

    operation Prepare(universe: Universe, qubits: Qubit[]) : Unit {
        // Identify literal qubits, these are the ones Grover is applied to:
        mutable literals = [];
        for r in u.GetRegisters(universe) {
            if (r.IsLiteral(r)) {
                set literals = literals + qubits[r.GetRange(r)];
            }
        }

        let oraclesCount = Length(u.GetOracles(universe));

        // If no oracles defined in this universe, no need to apply grover:
        if (oraclesCount == 0) {
            log.Info("No oracles found. Skipping Grover");
            ResetAll(literals);
            ApplyToEachA(H, literals);
        } else {
            // When doing Grover, always add a tracker Qubit. This makes sure
            // there are always at most 1/2 valid answers, and
            // we also use it to verify grover returned a valid row
            use tracker = Qubit();
            let (t, u1) = u.AddLiteral(1, universe);
            let u2 = u.AddOracle(ket.Oracle(_tracker_oracle(t, _, _)), u1);

            let all = qubits + [tracker];
            set literals += [tracker];

            let oracle = _uber_oracle(u2, _, _);

            grover.Apply(oracle, all, literals);
            Reset(tracker);
        }

        // Once the oracles have filtered the literals, apply the expressions once more
        // to have their values are reflected correctly in the universe.
        for e in u.GetOperators(universe) {
            e!(qubits);
        }

        if (log.DEBUG_ON()) {
            Message($"[Q#] Final state after Prepare: ");
            if (Length(qubits) > 1) {
                DumpMachine();
            }
            Message("");
        }
    }

    operation _tracker_oracle(tracker: r.Register, qubits: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        let t = qubits[r.GetRange(tracker)];
        log.Debug($".tracker. {t}");
        Controlled X(t, target);
    }

    operation _uber_oracle(universe: Universe, all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        let oracles = u.GetOracles(universe);
        let n = Length(oracles);
        
        log.Info($"Universe::uber-oracle --> oracles count: {n}");

        if (n == 1) {
            oracles[0]!(all, target);
        
        } else {
            use ancillas = Qubit[n];
            let pairs = Zipped(oracles, ancillas);

            within {
                for e in u.GetOperators(universe) {
                    e!(all);
                }

                for (o, a) in pairs {
                    o!(all, a);
                }
            }
            apply {
                Controlled X (ancillas, target);
            }
        }
    }
}