namespace ket {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Arrays;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Diagnostics;

    newtype Ket = (
        reset: Qubit[] => Unit,
        oracle: (Qubit[], Qubit) => Unit is Adj + Ctl,
        qubits: Qubit[],
        size: Int,
        register: Range[],
        valid: Qubit[] => Bool
    );

    newtype Ket2 = (tracker: Qubit, registers: Qubit[][]);

    operation _init_tuple(classic: Int[][], tracker: Qubit, quantum: Qubit[][]) : Ket
    {
        let (h, r) = HeadAndRest(quantum);
        let registers = Fold(_ranges, [0..Length(h)-1], r);

        let reset = _resetTuple;
        let oracle = _tupleOracle(classic, registers, _, _);
        let qubits = Fold(_flatten, [], quantum) + [tracker];
        let size = Length(classic);

        log.Info($"Ket Init --> classic: {classic}, quantum: {quantum}, qubits:{qubits}, registers: {registers}");

        return Ket(reset, oracle, qubits, size, registers, _validTuple);
    }


    operation Solve(k: Ket, tracker: Qubit) : Ket
    {
        let (_, _, qubits, size, r, _) = k!;

        let reset = _resetSolve;
        let oracle = _solveOracle(k, _, _);
        let registers = r[0..Length(r) -2];

        log.Info($"Solve Init --> registers: {registers}");

        return Ket(reset, oracle, qubits + [tracker], 1, registers, _validSolve(k, _));
    }

    operation Init_1(values: Int[], tracker: Qubit, registers: Qubit[]) : Ket {
        let q1 = registers;
        let quantum = [q1];
        let classic =  Mapped(_t1, values);

        return _init_tuple(classic, tracker, quantum);
    }

    operation Init_2(values: (Int, Int)[], tracker: Qubit, registers: (Qubit[], Qubit[])) : Ket {
        let (q1, q2) = registers;
        let quantum = [q1, q2];
        let classic = Mapped(_t2, values);

        return _init_tuple(classic, tracker, quantum);
    }

    operation Prepare(ket: Ket) : Unit 
    {
        let (reset, oracle, qubits, answers, _, valid) = ket!;

        repeat {
            reset(qubits);
            grover.Apply(oracle, qubits, answers);
        } until (valid(qubits));
    }


    operation Print(ket: Ket) : Unit 
    {
        Prepare(ket);

        let (_, _, qubits, _, registers, _) = ket!;
        let active = Fold(_ket_visible(qubits, _, _), [], registers);
        Message($"Ket: {qubits}, active: {active}");
        DumpRegister((), qubits);
    }

    operation _validTuple(qubits: Qubit[]) : Bool
    {
        let tracker = qubits[Length(qubits) - 1];
        let r = M(tracker);
        log.Info($"tuple tracker ({tracker}): {r}");
        return r == One;
    }

    operation _validSolve(original: Ket, register: Qubit[]) : Bool
    {
        let (_, _, _, _, registers, o_valid) = original!;

        let qubits = register[0..Length(register) - 2];
        let tracker = register[Length(register) - 1];
        let filter = qubits[registers[Length(registers) - 1]][0];

        let s = M(tracker);
        log.Info($"solve tracker ({tracker}): {s}");
        if (s == One) {
            let f = M(tracker);
            log.Info($"filter tracker ({filter}): {f}");
            if (f == One) {
                if (o_valid(qubits)) {
                    return true;
                }
            }
        }

        return false;
    }
    
    operation _resetTuple(qubits: Qubit[]) : Unit
    {
        ResetAll(qubits);
        ApplyToEach(H, qubits);
    }

    operation _resetSolve(qubits: Qubit[]) : Unit
    {
        ResetAll(qubits);
        ApplyToEach(H, qubits[0..Length(qubits)-2]);
    }

    
    operation _solveOracle(original: Ket, register: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        let (_, oracle, _, answers, registers, _) = original!;

        let qubits = register[0..Length(register) - 2];
        let tracker = register[Length(register) - 1];
        let filter = qubits[registers[Length(registers) - 1]];

        log.Debug($"original: {original!}");
        log.Debug($"tracker: {tracker}");
        log.Debug($"qubits: {qubits}");
        log.Info($"Solving on: {filter}");

        oracle(qubits, tracker);

        Controlled X ([tracker] + filter, target);
    }

    operation _tupleOracle(classic: Int[][], registers: Range[], all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {

        for i in 0..Length(classic) - 1 {
            let v = classic[i];

            within {
                for k in 0..Length(v)-1 {
                    let q = all[registers[k]];
                    let n = Length(q);
                    let bits = IntAsBoolArray(v[k], n);
                    log.Debug($"v:{v}, q:{q}, bits:{bits}, ");

                    for b in 0 .. n - 1 {
                        if (bits[b] == false) {
                            X(q[b]);
                        }
                    }
                }
            }
            apply {
                Controlled X (all, target);
            }
        }
    }

    function _ket_visible(all: Qubit[], x: Qubit[], next: Range) : Qubit[] {
        return x + all[next];
    }

    function _flatten(x: Qubit[], next: Qubit[]) : Qubit[] {
        return x + next;
    }

    function _t1<'T>(v1: 'T) : 'T[] {        
        return [v1];
    }

    function _t2<'T>(values: ('T, 'T)) : 'T[] {        
        let (v1, v2) = values;
        return [v1, v2];
    }

    function _ranges<'T>(x: Range[], next: 'T[]) : Range[] {
        let tail = Tail(x);
        let start = RangeEnd(tail)+1;
        let n = [start..start+Length(next)-1];

        return x + n;
    }
}