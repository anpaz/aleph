namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp;
    open aleph.qsharp.log as log;

    function Not(source: Register, previous: Universe) : (Universe, Register[])
    {
        let (oldRows, oldColumns, oldOracle) = previous!;

        let idx = oldColumns;
        let output = [Register(idx..idx)];

        let oracle = _Not_oracle(source, idx, _, _);
        let universe = Universe(oldRows, oldColumns + 1, oldOracle + [oracle]);

        log.Info($"Ket.Not::Init --> source: {source}; output: {output}");
        return (universe, output);
    }

    operation _Not_oracle(
        source: Register,
        idx: Int,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.Not::oracle --> target:{target}");
        
        let answer = all[idx];
        let ctrls = all[source!] + [answer];

        X (answer);
        Controlled X (ctrls, target);

        ApplyToEachCA(X, ctrls);
        Controlled X (ctrls, target);

        Adjoint ApplyToEachCA(X, ctrls);
        Adjoint X (answer);
    }
}