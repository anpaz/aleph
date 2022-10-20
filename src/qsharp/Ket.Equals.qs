namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.universe;
    open aleph.qsharp.register;
    open aleph.qsharp.log as log;

    function Equals(left: Register, right: Register, old: Universe) : (Universe, Register)
    {
        let (output, u) = AddExpressionOutput(1, old);
        let expr = _Equals_eval(left, right, output, _);
        let universe = AddExpression(expr, u);

        log.Info($"Ket.Equals::Init --> left: {left}; right: {right}; output: {output}");
        return (universe, output);
    }

    operation _Equals_eval(
        l: Register,
        r: Register,
        o: Register,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.Equals::eval --> output:{GetRange(o)}");
        
        let left = all[GetRange(l)];
        let right = all[GetRange(r)];
        let answer = all[GetRange(o)][0];

        // return false for all records if registers are of different size.
        if (Length(left) == Length(right)) {
            AreEqual(left, right, answer);
        }
    }

    operation AreEqual(left: Qubit[], right: Qubit[], answer: Qubit) : Unit
    is Adj + Ctl {
        if (Length(left) == Length(right)) {
            within {
                // Store in-place if the two qubits are equal
                for i in 0..Length(left) -1 {
                    CNOT(left[i], right[i]);
                    X(right[i]);
                }
            }
            apply {
                Controlled X (right, answer);
            }
        } else {
            fail ($"Invalid input for AreEqual: both registers must be of the same length");
        }
    }
}