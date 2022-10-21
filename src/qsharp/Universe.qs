namespace aleph.qsharp.universe {

    open aleph.qsharp.register;

    newtype Universe = (
        depth: Int,
        width: Int,
        registers: Register[],
        expressions: (Qubit[] => Unit is Adj + Ctl)[],
        oracles: ((Qubit[], Qubit) => Unit is Adj + Ctl)[]
    );

    function AddOracle(o: (Qubit[], Qubit) => Unit is Adj + Ctl, universe: Universe) : Universe {
        let (_,_,_,_,orcls) = universe!;
        return universe
            w/ oracles <- orcls + [o];
    }

    function AddExpression(e: Qubit[] => Unit is Adj + Ctl, universe: Universe) : Universe {
        let (_,_,_,exprs,_) = universe!;
        return universe
            w/ expressions <- exprs + [e];
    }

    function AddLiteral(size: Int, universe: Universe) : (Register, Universe) {
        return _addRegister(Literal, size, universe);
    }

    function AddExpressionOutput(size: Int, universe: Universe) : (Register, Universe) {
        return _addRegister(Expression, size, universe);
    }

    function _addRegister(ctr: Range -> Register, size: Int, old: Universe) : (Register, Universe) {
        let (_, cols, regs, _, _) = old!;

        let start = cols;
        let end = start + size - 1;

        let output = ctr(start..end);
        let universe = old
                w/ width <- cols + size
                w/ registers <- regs + [output];

        return (output, universe);
    }

    function GetDepth(universe: Universe) : Int {
        let (depth,_,_,_,_) = universe!;
        return depth;
    }

    function GetWidth(universe: Universe) : Int {
        let (_,width,_,_,_) = universe!;
        return width;
    }

    function GetRegisters(universe: Universe) : Register[] {
        let (_,_,registers,_,_) = universe!;
        return registers;
    }
    
    function GetExpressions(universe: Universe) : (Qubit[] => Unit is Adj + Ctl)[] {
        let (_,_,_,exprs,_) = universe!;
        return exprs;
    }

    function GetOracles(universe: Universe) : ((Qubit[], Qubit) => Unit is Adj + Ctl)[] {
        let (_,_,_,_,oracles) = universe!;
        return oracles;
    }
}