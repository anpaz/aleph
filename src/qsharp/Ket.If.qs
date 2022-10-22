namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Canon;
    
    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.log as log;

    function If(cond: r.Register, onTrue: r.Register, onFalse: r.Register, old: u.Universe) : (u.Universe, r.Register[])
    {
        let size = Max([r.GetSize(onTrue), r.GetSize(onFalse)]);
        let (output, u) = u.AddExpressionOutput(size, old);
        let expr = _If_eval(cond, onTrue, onFalse, output, _);
        let universe = u.AddExpression(expr, u);

        log.Info($"Ket.If::Init --> cond: {cond}; onTrue: {onTrue}; onFalse: {onFalse} output: {output}");
        return (universe, [output]);
    }

    operation _If_eval(
        c: r.Register,
        t: r.Register,
        e: r.Register,
        o: r.Register,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.If::eval");
        
        let cond_q = all[r.GetRange(c)];
        let then_q = all[r.GetRange(t)];
        let else_q = all[r.GetRange(e)];
        let out_q = all[r.GetRange(o)];

        // Copy then_q controlled on cond_q
        for i in 0 .. Length(then_q) - 1 {
            Controlled X (cond_q + [then_q[i]], out_q[i]);
        }

        // Negate condition
        ApplyToEachCA(X, cond_q);

        // Now copy else_q controlled on cond_q
        for i in 0 .. Length(else_q) - 1 {
            Controlled X (cond_q + [else_q[i]], out_q[i]);
        }

        // Undo condition:
        ApplyToEachCA(X, cond_q);
    }
}