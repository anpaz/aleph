namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Arithmetic;
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.log as log;
    open aleph.qsharp.register as r;
    open aleph.qsharp.universe;
    open aleph.qsharp.value;

    function Constant(v: Value, old: Universe) : (Universe, r.Register)
    {
        let (output, u) = AddExpressionOutput(GetSize(v), old);
        let expr = _Constant_eval(v, output, _);
        let universe = AddExpression(expr, u);

        log.Info($"Ket.Constant::Init --> value: {v}; output: {output}");
        return (universe, output);
    }

    operation _Constant_eval(
        v: Value,
        o: r.Register,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        let value = GetValue(v);
        let output = all[r.GetRange(o)];

        log.Debug($"Ket.Constant::eval --> value:{value}, output:{output}");
        ApplyXorInPlace(value, LittleEndian(output));
    }
}