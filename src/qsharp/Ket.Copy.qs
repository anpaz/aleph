namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Canon;
    
    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.log as log;

    function Copy(source: r.Register, target: r.Register) : Operator
    {
        log.Info($"Ket.Copy::Init --> source: {source}; target: {target}");
        return Operator(_Copy_eval(source, target, _));
    }

    operation _Copy_eval(
        s: r.Register,
        t: r.Register,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.Copy::eval");
        
        let source = all[r.GetRange(s)];
        let target = all[r.GetRange(t)];

        let n = Min([Length(source), Length(target)]);

        // Copy then_q controlled on cond_q
        for i in 0 .. n - 1 {
            Controlled X ([source[i]], target[i]);
        }
    }
}