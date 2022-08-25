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

        let oracle = _And_oracle(left, right, idx, oldOracle, _, _);
        let universe = Universe(oldRows, oldColumns + 1, oracle);

        log.Info($"Ket.And::Init --> left: {left}; right: {right}");
        return (universe, output);
    }

    operation _And_oracle(
        left: Register,
        right: Register,
        idx: Int,
        previous: (Qubit[], Qubit) => Unit is Adj + Ctl,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.All::oracle --> target:{target}");
        
        let answer = all[idx];
        use a1 = Qubit();

        use t1 = Qubit();
        use t2 = Qubit();

        within {
            previous(all, t1);

            // a1 hold true if left ^ right
            Controlled X (all[left!] + all[right!], a1);

            // the true cases
            Controlled X ([a1, answer], t2);

            // the false cases
            X(a1);
            X(answer);
            Controlled X ([a1, answer], t2);
        } apply {
            Controlled X ([t1, t2], target);
        }
    }
}