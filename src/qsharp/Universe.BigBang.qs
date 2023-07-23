namespace aleph.qsharp.universe {

    open Microsoft.Quantum.Intrinsic;

    open aleph.qsharp.register;
    open aleph.qsharp.log as log;

    function BigBang(): UniverseInfo {
        return UniverseInfo(0, [], [], []);
    }
}