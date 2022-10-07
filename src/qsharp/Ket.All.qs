namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp;
    open aleph.qsharp.log as log;

    function All(size: Int, previous: Universe) : (Universe, Register[])
    {
        let (oldRows, oldColumns, oldOracle) = previous!;

        let start = oldColumns;
        let end = start + size - 1;
        let output = [Register(start..end)];

        let rows = oldRows * (1 <<< size);

        let universe = Universe(rows, oldColumns + size, oldOracle);

        log.Info($"Ket.All::Init --> size: {size}; output: {output}");
        return (universe, output);
    }
}