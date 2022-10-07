namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp;
    open aleph.qsharp.log as log;

    function And(left: Register, right: Register, previous: Universe) : (Universe, Register[])
    {
        let (oldRows, oldColumns, oldOracle) = previous!;

        let idx = oldColumns;
        let output = [Register(idx..idx)];

        let oracle = _And_oracle(left, right, idx, _, _);
        let universe = Universe(oldRows, oldColumns + 1, oldOracle + [oracle]);

        log.Info($"Ket.And::Init --> left: {left}; right: {right}; output: {output}");
        return (universe, output);
    }

    operation _And_oracle(
        left: Register,
        right: Register,
        idx: Int,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.And::oracle --> target:{target}");
        
        let answer = all[idx];
        use a1 = Qubit();

        // a1 hold true if left ^ right
        Controlled X (all[left!] + all[right!], a1);

        // the true cases
        Controlled X ([a1, answer], target);

        // the false cases
        X(a1);
        X(answer);
        Controlled X ([a1, answer], target);
        
        // Undo
        Adjoint X(answer);
        Adjoint X(a1);
        Adjoint Controlled X (all[left!] + all[right!], a1);
    }
}