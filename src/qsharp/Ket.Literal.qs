namespace aleph.qsharp.ket {
    open aleph.qsharp;
    open aleph.qsharp.log as log;

    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Arrays;


    function Literal(classic: Value[][], oldUniverse: Universe) : Universe
    {
        let (oldRows, oldColumns, _, _) = oldUniverse!;
        let tupleSize = Length(classic[0]);

        mutable start = oldColumns;
        mutable output = [];
        for i in 0..tupleSize-1 {
            let (_, size) = classic[0][i]!;
            set output = output + [start..start+size-1];
            set start = start + size;
        }

        let rows = oldRows == 0 
            ? Length(classic)
            | oldRows * Length(classic);
        let columns = start;

        let oracle = _Literal_oracle(classic, output, oldColumns, start-1, _, _);
        let universe = Universe(rows, columns, output, oracle);

        log.Info($"Literal Init --> classic: {classic}, output: {output}");
        return universe;
    }

    operation _Literal_oracle(
        classic: Value[][],
        registers: Range[],
        first: Int,
        last: Int,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"all:{all}, target:{target}, first:{first}, last:{last}");

        for i in 0..Length(classic) - 1 {
            let value = classic[i];

            within {
                for k in 0..Length(value)-1 {
                    let (v, _) = value[k]!;
                    let q = all[registers[k]];
                    let n = Length(q);
                    let bits = IntAsBoolArray(v, n);
                    log.Debug($"v:{v}, q:{q}, bits:{bits}, ");

                    for b in 0 .. n - 1 {
                        if (bits[b] == false) {
                            X(q[b]);
                        }
                    }
                }
            } apply {
                let ctrls = all[first..last];
                Controlled X (ctrls, target);
            }
        }
    }
}