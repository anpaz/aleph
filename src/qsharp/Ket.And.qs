namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.log as log;

    function And(left: r.Register, right: r.Register, old: u.Universe) : (u.Universe, r.Register[])
    {
        let (output, u) = u.AddExpressionOutput(1, old);
        let expr = _And_eval(left, right, output, _);
        let universe = u.AddExpression(expr, u);

        log.Info($"Ket.And::Init --> left: {left}; right: {right}; output: {output}");
        return (universe, [output]);
    }

    operation _And_eval(
        l: r.Register,
        r: r.Register,
        a: r.Register,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        let left = r.GetRange(l);
        let right = r.GetRange(r);
        let answer = all[r.GetRange(a)];

        log.Debug($"Ket.And::eval --> left{left}, right{right}, answer:{answer}");        
        Controlled X (all[left] + all[right], answer[0]);
    }
}