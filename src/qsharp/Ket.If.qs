namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Canon;
    
    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.log as log;

    function If(cond: r.Register, onTrue: Qubit[] => Unit is Adj + Ctl, 
        onFalse: Qubit[] => Unit is Adj + Ctl) : Qubit[] => Unit is Adj + Ctl
    {
        log.Info($"Ket.If::Init --> cond: {cond}; onTrue: {onTrue}; onFalse: {onFalse}");
        return _If_eval(cond, onTrue, onFalse, _);
    }

    operation _If_eval(
        c: r.Register,
        t: Qubit[] => Unit is Adj + Ctl,
        e: Qubit[] => Unit is Adj + Ctl,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.If::eval");
        
        let cond_q = all[r.GetRange(c)];

        Controlled t (cond_q, all);

        // Negate condition
        ApplyToEachCA(X, cond_q);

        Controlled e (cond_q, all);

        // Undo condition:
        ApplyToEachCA(X, cond_q);
    }
}