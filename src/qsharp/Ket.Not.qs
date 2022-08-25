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

        let oracle = _Not_oracle(source, idx, oldOracle, _, _);
        let universe = Universe(oldRows, oldColumns + 1, oracle);

        log.Info($"Ket.Not::Init --> source: {source}");
        return (universe, output);
    }

    operation _Not_oracle(
        source: Register,
        idx: Int,
        previous: (Qubit[], Qubit) => Unit is Adj + Ctl,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.Not::oracle --> target:{target}");
        
        let answer = all[idx];
        let ctrls = all[source!] + [answer];

        use t1 = Qubit();
        use t2 = Qubit();

        within {
            previous(all, t1);

            X (answer);
            Controlled X (ctrls, t2);

            ApplyToEachCA(X, ctrls);
            Controlled X (ctrls, t2);
        } apply {
            Controlled X ([t1, t2], target);
        }
    }
}