namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.log as log;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.register as r;

    function Not(left: r.Register, old: u.Universe) : (u.Universe, r.Register[])
    {
        let (output, u) = u.AddExpressionOutput(1, old);
        let expr = _Not_eval(left, output, _);
        let universe = u.AddExpression(expr, u);

        log.Info($"Ket.Or::Init --> left: {left}; output: {output}");
        return (universe, [output]);
    }

    operation _Not_eval(
        l: r.Register,
        o: r.Register,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        let left = all[r.GetRange(l)];
        let output = all[r.GetRange(o)][0];

        log.Debug($"Ket.Not::eval --> left{left}, output:{output}");
        within {
            ApplyToEachCA(X, left);
        }
        apply {
            Controlled X (left, output);
        }
    }
}