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

        let oracle = _Equals_oracle(left, right, idx, _, _);
        let universe = Universe(oldRows, oldColumns + 1,  oldOracle + [oracle]);

        log.Info($"Ket.Equals::Init --> left: {left}; right: {right}; output: {output}");
        return (universe, output);
    }

    operation _Equals_oracle(
        l: Register,
        r: Register,
        idx: Int,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.Equals::oracle --> target:{target}");
        
        let left = all[l!];
        let right = all[r!];
        let answer = all[idx];

        // return false for all records if registers are of different size.
        if (Length(left) != Length(right)) {
            within {
                X (answer);
            } apply {
                Controlled X ([answer], target);
            }
        } else {
            let n = Length(left);
            use a1 = Qubit();

            AreEqual(left, right, a1);

            // the true cases.
            Controlled X ([a1, answer], target);

            // the false cases.
            X(a1);
            X(answer);
            Controlled X ([a1, answer], target);

            // Undo
            Adjoint X(answer);
            Adjoint X(a1);
            Adjoint AreEqual(left, right, a1);

        }
    }

    operation AreEqual(left: Qubit[], right: Qubit[], answer: Qubit) : Unit
    is Adj + Ctl {
        use a = Qubit[Length(left)];

        within {
        // a_i holds true if left_i == right_i
        for i in 0..Length(left) -1  {
            Controlled X ([left[i], right[i]], a[i]);
            X(left[i]);
            X(right[i]);
            Controlled X ([left[i], right[i]], a[i]);
        }
        } apply {
            Controlled X (a, answer);
        }
    }
}