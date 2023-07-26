namespace aleph.qsharp.register {

    open Microsoft.Quantum.Convert;

    newtype Register = (
        idx: Range,
        width: Int
    );

    function NewRegister(start: Int, width: Int) : Register {
        return Register(start..(start + width - 1), width);
    }

    // function NewLiteral(start: Int, width: Int) : Register {
    //     return Register(start..(start + width - 1), width, 0);
    // }

    // function NewOutput(start:Int, width: Int) : Register {
    //     return Register(start..(start + width - 1), width, 1);
    // }

    // function IsLiteral(r: Register) : Bool {
    //     let (_, _, type) = r!;
    //     return type == 0;
    // }

    // function IsOutput(r: Register) : Bool {
    //     let (_, _, type) = r!;
    //     return type == 1;
    // }

    function GetRange(r: Register) : Range {
        let(idx, _) = r!;
        return idx;
    }

    function GetWidth(r: Register) : Int {
        let(_, w) = r!;
        return w;
    }
}