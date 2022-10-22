namespace aleph.qsharp.universe {

    open Microsoft.Quantum.Diagnostics;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.register;
    open aleph.qsharp.universe as u;

    operation Print(universe: Universe) : Unit {
        let width = u.GetWidth(universe);
        use qubits = Qubit[width];

        // Prepare the universe, then applied the expressions so users 
        // can see their values too:
        Prepare(universe, qubits, 1);

        Message($"[Q#] Dump Universe: (qubits: {Length(qubits)})");
        if (Length(qubits) > 1) {
            DumpMachine(());
        }
        Message("");

        ResetAll(qubits);
    }
}