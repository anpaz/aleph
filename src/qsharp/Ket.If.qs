namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Arithmetic;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp;
    open aleph.qsharp.log as log;

    function If(c: Register, t: Register, e: Register, previous: Universe) : (Universe, Register[])
    {
        let (oldRows, oldColumns, oldOracle) = previous!;

        // Assume t/e conditions have same length
        let size = RangeEnd(t!) - RangeStart(t!) + 1;

        let idx = oldColumns;
        let output = Register(idx .. idx + size - 1);

        let oracle = _If_oracle(c, t, e, output, _, _);
        let universe = Universe(oldRows, oldColumns + size, oldOracle + [oracle]);

        log.Info($"Ket.If::Init --> cond: {c}; then: {t}; else: {e}; output: {output}");
        return (universe, [output]);
    }

    operation _If_oracle(
        c: Register,
        t: Register,
        e: Register,
        o: Register,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.If::oracle --> target:{target}");
        
        let cond_q = all[c!];
        let then_q = all[t!];
        let else_q = all[e!];
        let out_q = all[o!];

        use a1 = Qubit();
        use a2 = Qubit();

        AreEqual(then_q, out_q, a1);
        AreEqual(else_q, out_q, a2);

        // if cond is true, pick then qubits
        Controlled X (cond_q + [a1], target);
        // if cond is false, then pick else qubits
        ApplyToEachCA(X, cond_q);
        Controlled X (cond_q + [a2], target);

        // Undo
        Adjoint ApplyToEachCA(X, cond_q);
        Adjoint AreEqual(else_q, out_q, a2);
        Adjoint AreEqual(then_q, out_q, a1);
    }
}