namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Arithmetic as a;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.log as log;

    function GreaterThan(left: r.Register, right: r.Register, old: u.Universe) : (u.Universe, r.Register[])
    {
        let (output, u) = u.AddExpressionOutput(1, old);
        let expr = _GreaterThan_eval(left, right, output, _);
        let universe = u.AddExpression(expr, u);

        log.Info($"Ket.GreaterThan::Init --> left: {left}; right: {right}; output: {output}");
        return (universe, [output]);
    }

    operation _GreaterThan_eval(
        l: r.Register,
        r: r.Register,
        o: r.Register,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.GreaterThan::eval --> target:{o}");
        
        let left = all[r.GetRange(l)];
        let right = all[r.GetRange(r)];
        let output = all[r.GetRange(o)];

        let (lpadsize, rpadsize) = _padding(left, right);
        use lpad = Qubit[lpadsize];
        use rpad = Qubit[rpadsize];

        a.GreaterThan(a.LittleEndian(left + lpad), a.LittleEndian(right + rpad), output[0]);
    }
}