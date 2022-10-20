namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Arithmetic;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.universe;
    open aleph.qsharp.register as r;
    open aleph.qsharp.log as log;

    function Filter(c: r.Register, hint: Int, old: Universe) : Universe
    {
        let rowsHint = _rows_heuristic(hint, old);
        let oracle = _Filter_oracle(c, _, _);
        let universe = AddOracle(oracle, old) w/ rows <- rowsHint;

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

    function _rows_heuristic(hint: Int, universe: Universe) : Int {
        if (hint > 0) {
            return hint;
        }

        let result = GetRows(universe) >>> 1;

        if result <= 0 {
            return 1;
        }

        return result;
    }
}