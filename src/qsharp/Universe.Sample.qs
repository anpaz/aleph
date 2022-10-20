namespace aleph.qsharp.universe {

    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Arrays;
    open Microsoft.Quantum.Convert;

    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.value as v;

    operation Sample(u: Universe, o: r.Register[]) : v.Value[] {
        let (universe, output) = _map_outputs(u, o);
        let columns = u.GetColumns(universe);
        use qubits = Qubit[columns];

        Prepare(universe, qubits);

        mutable result = [];
        for r in output {
            let idx = r.GetRange(r);
            let size = r.GetSize(r);
            let value = ResultArrayAsInt(ForEach(M, qubits[idx]));
            set result += [ v.Value(value, size) ];
        }

        ResetAll(qubits);
        return result;
    }

    function _map_outputs(universe: Universe, outputs: r.Register[]) : (Universe, r.Register[]) {
        if (Length(outputs) == 0) {
            return (universe, outputs);
        } else {
            let head = Head(outputs);
            let rest = Rest(outputs);

            if (r.IsLiteral(head)) {
                let (u1, o1) = _map_outputs(universe, rest);
                return (u1, [head] + o1);
            } else {
                // Add a new output register for this expression:
                let n = r.GetSize(head);
                let (u1, o1) = aleph.qsharp.ket.All(n, universe);
                // Create and add anoracle that matches this register with the new output
                let oracle = _equal(head, o1, _, _);
                let u2 = u.AddOracle(oracle, u1);
                // Continue recursively:
                let (u3, o3) = _map_outputs(u2, rest);
                return (u3, [o1] + o3);
            }
        }
    }

    operation _equal(l: r.Register, r: r.Register, all: Qubit[], target: Qubit) : Unit 
    is Adj + Ctl {
        let left = r.GetRange(l);
        let right = r.GetRange(r);
        aleph.qsharp.ket.AreEqual(all[left], all[right], target);
    }
}