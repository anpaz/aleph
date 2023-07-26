namespace aleph.qsharp.universe {

    open aleph.qsharp.register;
    open aleph.qsharp.ket;

    newtype UniverseInfo = (
        width: Int,
        literals: Register[],
        operators: Operator[],
        oracles: Oracle[]
    );

    function AddOracle(o: Oracle, universe: UniverseInfo) : UniverseInfo {
        let (_,_,_,orcls) = universe!;
        return universe
            w/ oracles <- orcls + [o];
    }

    function AddOperator(e: Operator, universe: UniverseInfo) : UniverseInfo {
        let (_,_,ops,_) = universe!;
        return universe
            w/ operators <- ops + [e];
    }

    function AddLiteral(size: Int, universe: UniverseInfo) : (Register, UniverseInfo) {
        let (cols, regs, _, _) = universe!;

        let start = cols;

        let output = NewRegister(start, size);
        let u = universe
                w/ width <- cols + size
                w/ literals <- regs + [output];

        return (output, u);
    }

    function GetWidth(universe: UniverseInfo) : Int {
        let (width,_,_,_) = universe!;
        return width;
    }

    function GetLiterals(universe: UniverseInfo) : Register[] {
        let (_,lits,_,_) = universe!;
        return lits;
    }

    function GetOperators(universe: UniverseInfo) : Operator[] {
        let (_,_,ops,_) = universe!;
        return ops;
    }

    function GetOracles(universe: UniverseInfo) : Oracle[] {
        let (_,_,_,oracles) = universe!;
        return oracles;
    }
}