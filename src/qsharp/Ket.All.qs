namespace aleph.qsharp.ket {

    open aleph.qsharp.register as r;
    open aleph.qsharp.universe as u;
    open aleph.qsharp.log as log;

    function All(size: Int, old: u.Universe) : (u.Universe, r.Register[])
    {
        let (output, universe) = u.AddLiteral(size, old);

        log.Info($"Ket.All::Init --> size: {size}; output: {output}");
        return (universe w/ depth <- u.GetDepth(universe) * (1 <<< size), [output]);
    }
}