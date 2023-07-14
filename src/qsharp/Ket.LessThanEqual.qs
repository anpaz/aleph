namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Arithmetic as a;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.log as log;

    function LessThanEqual(left: r.Register, right: r.Register, output: r.Register) : Operator
    {
        log.Info($"Ket.LessThanEqual::Init --> left: {left}; right: {right}; output: {output}");
        return Operator(_LessThan_eval(left, right, output, _));
    }

    function _padding(left: Qubit[], right: Qubit[]) : (Int, Int) {
        let l = Max([Length(right) - Length(left), 0]);
        let r = Max([Length(left) - Length(right), 0]);

        log.Debug($"Using padding: left:{left} + {l}, right:{right} + {r}");

        return (l, r);
    }

    operation _LessThan_eval(
        l: r.Register,
        r: r.Register,
        o: r.Register,
        all: Qubit[]) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.LessThanEqual::eval --> target:{o}");
        
        let left = all[r.GetRange(l)];
        let right = all[r.GetRange(r)];
        let output = all[r.GetRange(o)];

        let (lpadsize, rpadsize) = _padding(left, right);
        use lpad = Qubit[lpadsize];
        use rpad = Qubit[rpadsize];

        a.GreaterThan(a.LittleEndian(left + lpad), a.LittleEndian(right + rpad), output[0]);
        X(output[0]);
    }
}