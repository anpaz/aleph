namespace aleph.qsharp.value {

    open Microsoft.Quantum.Convert;

    newtype Value = (
        value: Int,
        width: Int
    );

    function GetValue(v: Value) : Int {
        let(value, _) = v!;
        return value;
    }

    function GetWidth(v: Value) : Int {
        let(_, width) = v!;
        return width;
    }
}