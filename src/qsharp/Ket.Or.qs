namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp;
    open aleph.qsharp.log as log;

    function Or(left: Register, right: Register, previous: Universe) : (Universe, Register[])
    {
        let (oldRows, oldColumns, oldOracle) = previous!;

        let idx = oldColumns;
        let output = [Register(idx..idx)];

        let oracle = _Or_oracle(left, right, idx, _, _);
        let universe = Universe(oldRows, oldColumns + 1, oldOracle + [oracle]);

        log.Info($"Ket.Or::Init --> left: {left}; right: {right}; output: {output}");
        return (universe, output);
    }

    operation _Or_oracle(
        l: Register,
        r: Register,
        idx: Int,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.Or::oracle --> target:{target}");
        
        let answer = all[idx];
        let left = all[l!];
        let right = all[r!];
        use a1 = Qubit();

        // a1 hold true if left || right == !(!left ^ !right)
        ApplyToEachCA(X, left);
        ApplyToEachCA(X, right);
        Controlled X (left + right, a1);
        X (a1);

        // the true cases
        Controlled X ([a1, answer], target);

        // the false cases
        X(a1);
        X(answer);
        Controlled X ([a1, answer], target);

        // Undo
        // a1 hold true if left || right == !(!left ^ !right)

        Adjoint X(answer);
        Adjoint Controlled X (left + right, a1);
        Adjoint ApplyToEachCA(X, right);
        Adjoint ApplyToEachCA(X, left);
    }
}