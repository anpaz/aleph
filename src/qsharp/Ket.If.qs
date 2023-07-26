namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Canon;
    
    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.log as log;

    function If(cond: r.Register, onTrue: Operator, onFalse: Operator) : Operator
    {
        log.Info($"Ket.If::Init --> cond: {cond}; onTrue: {onTrue}; onFalse: {onFalse}");
        return Operator(_If_eval(cond, onTrue, onFalse, _));
    }

    operation _If_eval(
        c: r.Register,
        t: Operator,
        e: Operator,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.If::eval");
        
        let cond_q = all[r.GetRange(c)];
        let onTrue = t!;
        let onFalse = e!;

        // Apply onTrue controlled by cond
        Controlled onTrue (cond_q, all);

        // Negate condition
        ApplyToEachCA(X, cond_q);

        // Apply onFalse controlled by !cond
        Controlled onFalse (cond_q, all);

        // Undo condition:
        ApplyToEachCA(X, cond_q);
    }
}