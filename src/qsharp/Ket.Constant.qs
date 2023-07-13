namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Arithmetic;
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.log as log;
    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.value as v;

    function Constant(value: v.Value, output: r.Register) : (Qubit[] => Unit is Adj + Ctl)
    {
        log.Info($"Ket.Constant::Init --> value: {value}; output: {output}");
        return _Constant_eval(value, output, _);
    }

    operation _Constant_eval(
        v: v.Value,
        o: r.Register,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        let value = v.GetValue(v);
        let output = all[r.GetRange(o)];

        log.Debug($"Ket.Constant::eval --> value:{value}, output:{output}");
        ApplyXorInPlace(value, LittleEndian(output));
    }
}