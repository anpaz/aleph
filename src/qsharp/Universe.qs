namespace aleph.qsharp.universe {

    open aleph.qsharp.register;
    open aleph.qsharp.ket;

    newtype UniverseInfo = (
        width: Int,
        registers: Register[],
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
        return _addRegister(NewLiteral, size, universe);
    }

    function AddOutput(size: Int, universe: UniverseInfo) : (Register, UniverseInfo) {
        return _addRegister(NewOutput, size, universe);
    }

    function _addRegister(ctr: (Int, Int) -> Register, size: Int, old: UniverseInfo) : (Register, UniverseInfo) {
        let (cols, regs, _, _) = old!;

        let start = cols;

        let output = ctr(start, size);
        let universe = old
                w/ width <- cols + size
                w/ registers <- regs + [output];

        return (output, universe);
    }

    function GetWidth(universe: UniverseInfo) : Int {
        let (width,_,_,_) = universe!;
        return width;
    }

    function GetRegisters(universe: UniverseInfo) : Register[] {
        let (_,registers,_,_) = universe!;
        return registers;
    }
    
    function GetOperators(universe: UniverseInfo) : Operator[] {
        let (_,_,exprs,_) = universe!;
        return exprs;
    }

    function GetOracles(universe: UniverseInfo) : Oracle[] {
        let (_,_,_,oracles) = universe!;
        return oracles;
    }
}