namespace ket {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Arrays;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Diagnostics;

    newtype Ket = (tracker: Qubit, registers: Qubit[][]);

    operation _init(classic: Int[][],  tracker: Qubit, quantum: Qubit[][]) : Unit
    {
        let all =  Fold(_flatten, [], quantum) + [tracker];

        // // For debugging:
        // use t = Qubit();
        // ResetAll(all);
        // ApplyToEach(H, all);
        // let ketOracle = _tupleOracle(classic, tracker, quantum, t);
        // //DumpRegister((), all);
        // DumpMachine();
        // Reset(t);
        // Message($"Init --> classic: {classic}, quantum: {quantum}");

        // In case Grover fails to find a marked value
        repeat {
            Message($"Init --> classic: {classic}, quantum: {quantum}");
            ResetAll(all);
            ApplyToEach(H, all);
            let ketOracle = _grover_wrapper(_tupleOracle(classic, tracker, quantum, _), _, _);
            grover.Find(ketOracle, all, Length(classic));

        } 
        until M(tracker) == One
        fixup {
            Message("!!Repeat Grover!!");
        }
    }

    operation Init_1(values: Int[], tracker: Qubit, registers: Qubit[]) : Ket {
        let q1 = registers;
        let quantum = [q1];
        let classic =  Mapped(_t1, values);

        _init(classic, tracker, quantum);

        return Ket(tracker, quantum);
    }

    operation Init_2(values: (Int, Int)[], tracker: Qubit, registers: (Qubit[], Qubit[])) : Ket {
        let (q1, q2) = registers;
        let quantum = [q1, q2];
        let classic = Mapped(_t2, values);

        _init(classic, tracker, quantum);

        return Ket(tracker, quantum);
    }

    operation Print(ket: Ket) : Unit 
    is Adj {
        let (tracker, registers) = ket!;
        let qubits = Fold(_flatten, [], registers);
        Message($"Ket: {qubits}");
        DumpRegister((), qubits + [tracker]);
    }
    
    operation Solve(ket: Ket) : Ket
    {
        let (m1, r1) = ket!;
        let l = Length(r1);
        let t2 = r1[l-1][0];

        let qubits = Fold(_flatten, [], r1);

        grover.Find(_solverOracle(t2, _, _), r1[0], 1);
        Message($"m1:{m1}, t2:{t2}");
        DumpRegister((), qubits + [m1]);

//         Message("");
//         DumpMachine();

//         Reset(a);

//        Print(ket);

        let v = M(m1);
        //Reset(a);
        Message($"Measured: {v}");
        // if (v == Zero) {
        //     fail "Invalid measurement";
        // }

        //Print(ket);

        return Ket(t2, r1[0..l-2]);
    }
    
    operation _solverOracle(r: Qubit, _1: Qubit[], target: Qubit) : Unit
    is Adj {
        //X(target);
        //CNOT(tracker, target);
        CNOT(r, target);
    }



    operation _tupleOracle(classic: Int[][], tracker: Qubit, quantum: Qubit[][], target: Qubit) : Unit
    is Adj + Ctl {

        for i in 0..Length(classic) - 1 {
            let v = classic[i];

            within {
                for k in 0..Length(v)-1 {
                    let q = quantum[k];
                    let n = Length(q);
                    let bits = IntAsBoolArray(v[k], n);
                    //Message($"v: {v}, q: {q}, bits:{bits}, ");

                    for b in 0 .. n - 1 {
                        if (bits[b] == false) {
                            X(q[b]);
                        }
                    }
                }
            }
            apply {
                let all = [tracker] + Fold(_flatten, [], quantum);
                //Message($"all: {all}");
                Controlled X (all, target);
            }
        }
    }

    operation _grover_wrapper(p: (Qubit) => Unit is Adj, g1: Qubit[], target: Qubit) : Unit
    is Adj {
        p(target);
    }

    function _flatten(x: Qubit[], next: Qubit[]) : Qubit[] {
        return next + x;
    }

    function _t1<'T>(v1: 'T) : 'T[] {        
        return [v1];
    }

    function _t2<'T>(values: ('T, 'T)) : 'T[] {        
        let (v1, v2) = values;
        return [v1, v2];
    }
}