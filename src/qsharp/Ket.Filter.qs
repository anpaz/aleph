namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Arithmetic;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.universe as u;
    open aleph.qsharp.register as r;
    open aleph.qsharp.log as log;

    function Filter(c: r.Register, old: u.Universe) : u.Universe
    {
        let oracle = _Filter_oracle(c, _, _);
        let universe = u.AddOracle(oracle, old);

        log.Info($"Ket.Filter::Init --> cond: {r.GetRange(c)}");
        return universe;
    }

    operation _Filter_oracle(
        c: r.Register,
        all: Qubit[],
        target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.Filter::oracle --> target:{target}");
        
        let cond_q = all[r.GetRange(c)];
        Controlled X (cond_q, target);
    }
}