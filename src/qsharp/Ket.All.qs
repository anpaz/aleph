namespace aleph.qsharp.ket {

    open aleph.qsharp.register;
    open aleph.qsharp.universe;
    open aleph.qsharp.log as log;

    function All(size: Int, old: Universe) : (Universe, Register)
    {
        let (output, universe) = AddLiteral(size, old);

        log.Info($"Ket.All::Init --> size: {size}; output: {output}");
        return (universe w/ rows <- GetRows(universe) * (1 <<< size), output);
    }
}