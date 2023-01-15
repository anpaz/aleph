namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.value as v;
    open aleph.qsharp.log as log;

    function InSet(values: v.Value[][], registers: r.Register[], old: u.Universe) : (u.Universe, r.Register[])
    {
        let (output, u) = u.AddExpressionOutput(1, old);
        let expr = _In_eval(values, registers, output, _);
        let universe = u.AddExpression(expr, u);

        log.Info($"Ket.In::Init --> values: {values}, registers: {registers}, output: {output}");
        return (universe, [output]);
    }

    function get_controls (registers: r.Register[], all: Qubit[]) : Qubit[] {
        mutable ctrls = [];

        for r in registers {
            set ctrls += all[r.GetRange(r)];
        }
        
        return ctrls;
    }

    operation _In_eval(
        values: v.Value[][],
        registers: r.Register[],
        answer: r.Register,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.In::eval: registers:{registers}, answer:{answer}");

        let ctrls = get_controls(registers, all);
        let target = all[r.GetRange(answer)][0];

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
                Controlled X (ctrls, target);
            }
        }
    }
}