namespace aleph.qsharp.ket {

    open Microsoft.Quantum.Measurement;
    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Characterization;
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Arrays;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Diagnostics;

    open aleph.qsharp.grover as grover;
    open aleph.qsharp.log as log;

    newtype Ket = (
        oracle: (Qubit[], Qubit) => Unit is Adj + Ctl,
        universe: UniverseInfo
    );

    newtype UniverseInfo = (
        rows: Int,
        columns: Int,
        output: Range[]
    );

    operation Sample(ket: Ket) : QValue[] {
        let (oracle, universe) = ket!;
        let (_, columns, output) = universe!;

        use qubits = Qubit[columns + 1]; // One extra for tracker

        Prepare(ket, qubits);

        mutable result = [];
        for r in output {
            let value = ResultArrayAsInt(ForEach(M, qubits[r]));
            let size = Length(RangeAsIntArray(r));
            set result += [ QValue(value, size) ];
        }

        ResetAll(qubits);
        return result;
    }

    operation Wrapper (trackerIdx: Int, oracle: (Qubit[], Qubit) => Unit is Adj, qubits: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        use a = Qubit();

        within {
            oracle(qubits, a);
        } apply {
            Controlled X([qubits[trackerIdx], a], target);
        }
    }

    operation Prepare(ket: Ket, qubits: Qubit[]) : Unit {
        let (oracle, universe) = ket!;
        let (rows, columns, _output) = universe!;

        repeat {
            ResetAll(qubits);
            ApplyToEachA(H, qubits);
            grover.Apply(Wrapper(columns, oracle, _, _), qubits, rows);
        } until ((M(qubits[columns]) == One) or (rows == 0));

        if (log.INFO_ON()) {
            Message($"[Q#] Final state after Prepare: ");
            DumpMachine();
            Message("");
        }
    }
}