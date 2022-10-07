namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Arithmetic;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp;
    open aleph.qsharp.log as log;

    function Add(left: Register, right: Register, previous: Universe) : (Universe, Register[])
    {
        let (oldRows, oldColumns, oldOracles) = previous!;

        let size = RangeEnd(right!) - RangeStart(right!) + 1;

        let idx = oldColumns;
        let output = Register(idx .. idx + size - 1);

        let oracle = _Add_oracle(left, right, output, _, _);
        let universe = Universe(oldRows, oldColumns + size, oldOracles + [oracle]);

        log.Info($"Ket.Add::Init --> left: {left}; right: {right}; output: {output}");
        return (universe, [output]);
    }

    operation _Add_oracle(
        l: Register,
        r: Register,
        o: Register,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.Add::oracle --> target:{target}");
        
        let left = all[l!];
        let right = all[r!];
        let output = all[o!];

        within {
            AddI(LittleEndian(left), LittleEndian(right));
        } apply {
            AreEqual(right, output, target);
        }
    }
}