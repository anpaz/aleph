namespace aleph.qsharp.value {

    open Microsoft.Quantum.Convert;

    newtype Value = (
        value: Int,
        size: Int
    );

    function GetValue(v: Value) : Int {
        let(value, _) = v!;
        return value;
    }

    function GetSize(v: Value) : Int {
        let(_, size) = v!;
        return size;
    }
}