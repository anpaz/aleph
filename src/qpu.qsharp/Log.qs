
namespace aleph.qsharp.log
{
    open Microsoft.Quantum.Intrinsic;

    function INFO_ON() : Bool {
        return true;
    }

    function DEBUG_ON() : Bool {
        return false;
    }

    function Info(msg: String) : Unit {
        if (INFO_ON()) {
            Message(msg);
        }
    }

    function Debug(msg: String) : Unit {
        if (DEBUG_ON()) {
            Message(msg);
        }
    }
}
