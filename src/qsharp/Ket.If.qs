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

        let oracle = _If_oracle(c, t, e, output, oldOracle, _, _);
        let universe = Universe(oldRows, oldColumns + size, oracle);

        log.Info($"Ket.If::Init --> cond: {c}; then: {t}; else: {e}; output: {output}");
        return (universe, [output]);
    }

    operation _If_oracle(
        c: Register,
        t: Register,
        e: Register,
        o: Register,
        previous: (Qubit[], Qubit) => Unit is Adj + Ctl,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.All::oracle --> target:{target}");
        
        let cond_q = all[c!];
        let then_q = all[t!];
        let else_q = all[e!];
        let out_q = all[o!];

        use t1 = Qubit();
        use t2 = Qubit();
        use t2a = Qubit();
        use t2b = Qubit();

        within {
            previous(all, t1);

            AreEqual(then_q, out_q, t2a);
            AreEqual(else_q, out_q, t2b);

            // if cond is true, pick then qubits
            Controlled X (cond_q + [t2a], t2);
            // if cond is false, then pick else qubits
            ApplyToEachCA(X, cond_q);
            Controlled X (cond_q + [t2b], t2);

        } apply {
            Controlled X ([t1, t2], target);
        }
    }
}