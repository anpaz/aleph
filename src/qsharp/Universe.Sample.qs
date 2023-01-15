namespace aleph.qsharp.universe {

    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Arrays;
    open Microsoft.Quantum.Convert;

    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.value as v;

    operation Sample(universe: Universe, outputs: r.Register[]) : v.Value[] {
        let width = u.GetWidth(universe);
        use qubits = Qubit[width];

        Prepare(universe, qubits);

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