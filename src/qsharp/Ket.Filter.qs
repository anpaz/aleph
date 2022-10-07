namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Arithmetic;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp;
    open aleph.qsharp.log as log;

    function Filter(c: Register, previous: Universe) : (Universe, Register[])
    {
        let (oldRows, oldColumns, oldOracle) = previous!;

        let rows = _calculate_rows(oldRows);

        let oracle = _Filter_oracle(c, _, _);
        let universe = Universe(rows, oldColumns, oldOracle + [oracle]);

        log.Info($"Ket.Filter::Init --> cond: {c}");
        return (universe, []);
    }

    operation _Filter_oracle(
        c: Register,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.Filter::oracle --> target:{target}");
        
        let cond_q = all[c!];
        Controlled X (cond_q, target);
    }

    function _calculate_rows(oldRows: Int) : Int {
        let result = oldRows >>> 1;

        if result <= 0 {
            return 1;
        }

        return result;
    }

}