namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Arithmetic;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp;
    open aleph.qsharp.log as log;

    function Multiply(left: Register, right: Register, previous: Universe) : (Universe, Register[])
    {
        let (oldRows, oldColumns, oldOracle) = previous!;

        let size = (RangeEnd(left!) - RangeStart(left!) + 1) + 
                   (RangeEnd(right!) - RangeStart(right!) + 1);

        let idx = oldColumns;
        let output = Register(idx .. idx + size - 1);

        let oracle = _Multiply_oracle(left, right, output, _, _);
        let universe = Universe(oldRows, oldColumns + size, oldOracle + [oracle]);

        log.Info($"Ket.Add::Init --> left: {left}; right: {right}; output: {output}");
        return (universe, [output]);
    }

    operation _Multiply_oracle(
        l: Register,
        r: Register,
        o: Register,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.Multiply::oracle --> target:{target}");
        
        let left = all[l!];
        let right = all[r!];
        let output = all[o!];

        use a = Qubit[Length(output)];

        within {
            MultiplyI(LittleEndian(left), LittleEndian(right), LittleEndian(a));
        } apply {
            AreEqual(a, output, target);
        }
    }
}