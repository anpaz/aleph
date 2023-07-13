namespace aleph.qsharp.register {

    open Microsoft.Quantum.Convert;

    newtype Register = (
        idx: Range,
        type: Int
    );

    function NewLiteral(start: Int, end: Int) : Register {
        return Register(start..end, 0);
    }

    function NewOutput(start:Int, end: Int) : Register {
        return Register(start..end, 1);
    }

    function IsLiteral(r: Register) : Bool {
        let (_, type) = r!;
        return type == 0;
    }

    function IsOutput(r: Register) : Bool {
        let (_, type) = r!;
        return type == 1;
    }

    function GetRange(r: Register) : Range {
        let(idx, _) = r!;
        return idx;
    }

    function GetSize(r: Register) : Int {
        let(idx, _) = r!;
        return Length(RangeAsIntArray(idx));
    }
}