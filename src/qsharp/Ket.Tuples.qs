namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.value as v;
    open aleph.qsharp.log as log;

    function Tuples(values: v.Value[][], old: u.Universe) : (u.Universe, r.Register[])
    {
        let width = Length(values[0]);

        // Add a literal for every item in the tuple
        mutable u = old;
        mutable outputs = [];
        for i in 0..width-1 {
            let (r_i, u_i) = u.AddLiteral(v.GetSize(values[0][i]), u);
            set outputs = outputs + [r_i];
            set u = u_i;
        }

        let start = u.GetWidth(old);
        let end = u.GetWidth(u) - 1;
        let oracle = _Tuples_oracle(values, outputs, start, end, _, _);
        let universe = u.AddOracle(oracle, u);

        log.Info($"Ket.Tuples::Init --> values: {values}, output: {outputs}");
        return (universe, outputs);
    }

    operation _Tuples_oracle(
        values: v.Value[][],
        registers: r.Register[],
        first: Int,
        last: Int,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.Tuples::oracle: target:{target}, first:{first}, last:{last}");

        for i in 0..Length(values) - 1 {
            within {
                let item_i = values[i];
                for k in 0..Length(item_i)-1 {
                    let v = v.GetValue(item_i[k]);
                    let qubits = all[r.GetRange(registers[k])];
                    let n = Length(qubits);
                    let bits = IntAsBoolArray(v, n);
                    for b in 0 .. n - 1 {
                        if (bits[b] == false) {
                            X(qubits[b]);
                        }
                    }
                }
            }
            apply {
                let ctrls = all[first..last];
                Controlled X (ctrls, target);
            }
        }
    }
}