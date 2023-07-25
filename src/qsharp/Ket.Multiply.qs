namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Arithmetic;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.log as log;

    function Multiply(left: r.Register, right: r.Register, output: r.Register) : Operator
    {
        log.Info($"Ket.Multiply::Init --> left: {left}; right: {right}; output: {output}");
        return Operator(_Multiply_eval(left, right, output, _));
    }

    operation _Multiply_eval(
        l: r.Register,
        r: r.Register,
        o: r.Register,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        log.Info($"Ket.Multiply::eval --> l:{l}, r:{r}, o:{o}");
        
        let left = all[r.GetRange(l)];
        let right = all[r.GetRange(r)];
        let output = all[r.GetRange(o)];

        MultiplyI(LittleEndian(left), LittleEndian(right), LittleEndian(output));
    }
}