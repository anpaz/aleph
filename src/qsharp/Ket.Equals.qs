namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp;
    open aleph.qsharp.log as log;

    function Equals(left: Register, right: Register, previous: Universe) : (Universe, Register[])
    {
        let (oldRows, oldColumns, oldOracle) = previous!;

        let idx = oldColumns;
        let output = [Register(idx..idx)];

        let oracle = _Equals_oracle(left, right, idx, oldOracle, _, _);
        let universe = Universe(oldRows, oldColumns + 1, oracle);

        log.Info($"Ket.Equals::Init --> left: {left}; right: {right}");
        return (universe, output);
    }

    operation _Equals_oracle(
        l: Register,
        r: Register,
        idx: Int,
        previous: (Qubit[], Qubit) => Unit is Adj + Ctl,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.Equals::oracle --> target:{target}");
        
        let left = all[l!];
        let right = all[r!];
        let answer = all[idx];

        // return false for all records if registers are of different size.
        if (Length(left) != Length(right)) {
            use t1 = Qubit();
            within {
                previous(all, t1);
                X (answer);
            } apply {
                Controlled X ([t1, answer], target);
            }
        } else {
            let n = Length(left);
            use t1 = Qubit();
            use t2 = Qubit();
            use a1 = Qubit();
            use a = Qubit[n];

            within {
                previous(all, t1);
                
                let ctrls = left + right;

                // a1 holds true if left == right
                for i in 0..Length(left) -1  {
                    Controlled X ([left[i], right[i]], a[i]);
                    X(left[i]);
                    X(right[i]);
                    Controlled X ([left[i], right[i]], a[i]);
                }
                Controlled X (a, a1);

                // the true cases.
                Controlled X ([a1, answer], t2);

                // the false cases.
                X(a1);
                X(answer);
                Controlled X ([a1, answer], t2);
            } apply {
                Controlled X ([t1, t2], target);
            }
        }
    }
}