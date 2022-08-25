namespace aleph.qsharp {

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

    newtype Universe = (
        rows: Int,
        columns: Int,
        output: Range[],
        oracle: (Qubit[], Qubit) => Unit is Adj + Ctl
    );

    newtype Value = (
        value: Int,
        size: Int
    );
    
    operation Sample(universe: Universe) : Value[] {
        let (_, columns, output, oracle) = universe!;

        use qubits = Qubit[columns + 1]; // One extra for tracker

        Prepare(universe, qubits);

        mutable result = [];
        for r in output {
            let value = ResultArrayAsInt(ForEach(M, qubits[r]));
            let size = Length(RangeAsIntArray(r));
            set result += [ Value(value, size) ];
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

    operation Prepare(universe: Universe, qubits: Qubit[]) : Unit {
        let (rows, columns, _, oracle) = universe!;

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