namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Arithmetic;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.log as log;

    function Add(left: r.Register, right: r.Register, width: Int, old: u.Universe) : (u.Universe, r.Register[])
    {
        let (output, u) = u.AddExpressionOutput(width, old);
        let expr = _Add_eval(left, right, output, _);
        let universe = u.AddExpression(expr, u);

        log.Info($"Ket.Add::Init --> left: {left}; right: {right}; output: {output}");
        return (universe, [output]);
    }

    operation _Add_eval(
        l: r.Register,
        r: r.Register,
        o: r.Register,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.Add::eval --> target:{o}");
        
        let left = all[r.GetRange(l)];
        let right = all[r.GetRange(r)];
        let output = all[r.GetRange(o)];

        // AddI is in-place, copy right into output first:
        for i in 0 .. r.GetSize(r) - 1 {
            CNOT(right[i], output[i]);
        }

        AddI(LittleEndian(left), LittleEndian(output));
    }
}