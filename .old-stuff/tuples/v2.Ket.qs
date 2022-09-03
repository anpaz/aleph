namespace ket.v2 {

    open Microsoft.Quantum.Measurement;
    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Characterization;
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Arrays;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Diagnostics;

    newtype Ket = (
        //init: Qubit[] => Unit is Adj,
        //oracle: (Qubit[], Qubit) => Unit is Adj + Ctl,
        prepare: Qubit[] => Unit,
        size: Int,
        //answers: Int,
        registers: Range[]
        //tracker: Range,
        //valid: Qubit[] => Bool
    );


    operation Print(ket: Ket) : Unit 
    {
        //let (init, oracle, size,  answers, registers, tracker, _) = ket!;
        let (prepare, size, registers) = ket!;

        use qubits = Qubit[size];        
        prepare(qubits);

        // Unentangle from qubits outside the output registers:
        _unentangle(registers, qubits);

        log.Info($"Ket: {qubits}");
        mutable ordered = [];
        for r in registers {
            log.Info($"  {qubits[r]}");
            set ordered += qubits[r];
        }

        DumpRegister((), ordered);

        ResetAll(qubits);
    }

    // operation Sample(ket: Ket) : Int[]
    // {
    //     let (init, oracle, size,  answers, registers, tracker, _) = ket!;
    //     use qubits = Qubit[size];
        
    //     Prepare(ket, qubits);

    //     mutable result = [];
    //     for r in registers {
    //         set result += [ResultArrayAsInt(ForEach(M, qubits[r]))];
    //     }

    //     return result;
    // }


    // operation AskOracle(ket: Ket) : Unit 
    // {
    //     let (init, oracle, size,  answers, registers, tracker, _) = ket!;

    //     use qubits = Qubit[size];
    //     use t = Qubit();
    //     let all = qubits + [t];

    //     init(qubits);
    //     oracle(qubits, t);

    //     Message($"All: {qubits}");
    //     mutable ordered = [];
    //     for r in registers {
    //         log.Info($"  {qubits[r]}");
    //         set ordered += qubits[r];
    //     }
    //     log.Info($"  tracker: {qubits[tracker]}");
    //     log.Info($"  answer:  [{t}]");
    //     set ordered += qubits[tracker];
    //     set ordered += [t];

    //     DumpRegister((), ordered);

    //     ResetAll(all);
    // }


    // operation Prepare(ket: Ket, qubits: Qubit[]) : Unit 
    // {
    //     let (init, oracle, size, answers, _, _, valid) = ket!;

    //     repeat {
    //         ResetAll(qubits);
    //         init(qubits);
    //         grover.Apply(oracle, qubits, answers);
    //     } until (answers == 0 or valid(qubits));
    // }

    function _in_output(index: Int,  registers: Range[], qubits: Qubit[]) : Bool {
        for r in registers {
            for i in r {
                if index == i {
                    return true;
                }
            }
        }
        return false;
    }

    operation _unentangle(registers: Range[], qubits: Qubit[]) : Unit {
        for i in 0..Length(qubits)-1 {
            if not _in_output(i, registers, qubits) {
                let v = M(qubits[i]);
                log.Info($"unentangle: {i}: {v}");
            }
        }
    }

    function _ranges<'T>(x: Range[], next: 'T[]) : Range[] {
        let tail = Tail(x);
        let start = RangeEnd(tail)+1;
        let n = [start..start+Length(next)-1];

        return x + n;
    }
}