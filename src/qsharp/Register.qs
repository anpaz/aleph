namespace aleph.qsharp.register {

    open Microsoft.Quantum.Convert;

    newtype Register = (
        idx: Range,
        type: Int
    );

    function Literal(idx: Range) : Register {
        return Register(idx, 0);
    }

    function Expression(idx: Range) : Register {
        return Register(idx, 1);
    }

    function IsLiteral(r: Register) : Bool {
        let (_, type) = r!;
        return type == 0;
    }

    function IsExpression(r: Register) : Bool {
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