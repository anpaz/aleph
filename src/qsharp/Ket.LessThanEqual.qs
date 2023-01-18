namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Arithmetic as a;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.log as log;

    function LessThanEqual(left: r.Register, right: r.Register, old: u.Universe) : (u.Universe, r.Register[])
    {
        let (output, u) = u.AddExpressionOutput(1, old);
        let expr = _LessThan_eval(left, right, output, _);
        let universe = u.AddExpression(expr, u);

        log.Info($"Ket.LessThanEqual::Init --> left: {left}; right: {right}; output: {output}");
        return (universe, [output]);
    }

    operation _LessThan_eval(
        l: r.Register,
        r: r.Register,
        o: r.Register,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.LessThanEqual::eval --> target:{o}");
        
        let left = all[r.GetRange(l)];
        let right = all[r.GetRange(r)];
        let output = all[r.GetRange(o)];

        a.GreaterThan(a.LittleEndian(left), a.LittleEndian(right), output[0]);
        X(output[0]);
    }
}