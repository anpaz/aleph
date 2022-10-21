namespace aleph.qsharp.universe {

    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Arrays;
    open Microsoft.Quantum.Convert;

    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.value as v;

    operation Sample(universe: Universe, outputs: r.Register[], maxTries: Int) : v.Value[] {
        let columns = u.GetColumns(universe);
        use qubits = Qubit[columns];

        Prepare(universe, qubits, maxTries);

        mutable result = [];
        for r in outputs {
            let idx = r.GetRange(r);
            let size = r.GetSize(r);
            let value = ResultArrayAsInt(ForEach(M, qubits[idx]));
            set result += [ v.Value(value, size) ];
        }

        ResetAll(qubits);
        return result;
    }
}