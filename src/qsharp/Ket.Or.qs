namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.register as r;
    open aleph.qsharp.universe;
    open aleph.qsharp.log as log;

    function Or(left: r.Register, right: r.Register, output: r.Register) : Operator
    {
        log.Info($"Ket.Or::Init --> left: {left}; right: {right}; output: {output}");
        return Operator(_Or_eval(left, right, output, _));
    }

    operation _Or_eval(
        l: r.Register,
        r: r.Register,
        o: r.Register,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        let left = all[r.GetRange(r)];
        let right = all[r.GetRange(l)];
        let output = all[r.GetRange(o)][0];

        // left || right <=> !(!left ^ !right)
        log.Debug($"Ket.Or::eval --> left{left}, right{right}, answer:{output}");
        
        within {
            ApplyToEachCA(X, left);
            ApplyToEachCA(X, right);
        }
        apply {
            Controlled X (left + right, output);
            X (output);
        }
    }
}