namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Arithmetic;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp;
    open aleph.qsharp.log as log;

    function Add(left: Register, right: Register, previous: Universe) : (Universe, Register[])
    {
        let (oldRows, oldColumns, oldOracle) = previous!;

        let size = RangeEnd(right!) - RangeStart(right!) + 1;

        let idx = oldColumns;
        let output = Register(idx .. idx + size - 1);

        let oracle = _Add_oracle(left, right, output, oldOracle, _, _);
        let universe = Universe(oldRows, oldColumns + size, oracle);

        log.Info($"Ket.Add::Init --> left: {left}; right: {right}; output: {output}");
        return (universe, [output]);
    }

    operation _Add_oracle(
        l: Register,
        r: Register,
        o: Register,
        previous: (Qubit[], Qubit) => Unit is Adj + Ctl,
        all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        log.Debug($"Ket.All::oracle --> target:{target}");
        
        let left = all[l!];
        let right = all[r!];
        let output = all[o!];

        use t1 = Qubit();
        use t2 = Qubit();

        within {
            previous(all, t1);

            AddI(LittleEndian(left), LittleEndian(right));
            AreEqual(right, output, t2);
        } apply {
            Controlled X ([t1, t2], target);
        }
    }
}