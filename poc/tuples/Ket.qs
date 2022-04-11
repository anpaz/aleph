namespace ket {

    open Microsoft.Quantum.Measurement;
    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Characterization;
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Arrays;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Diagnostics;

    newtype Ket = (
        init: Qubit[] => Unit is Adj,
        oracle: (Qubit[], Qubit) => Unit is Adj + Ctl,
        size: Int,
        answers: Int,
        register: Range[],
        valid: Qubit[] => Bool
    );

    newtype Ket2 = (tracker: Qubit, registers: Qubit[][]);

    operation _init_tuple(classic: Int[][]) : Ket
    {
        let REGISTER_SIZE=2;
        let tupleSize = Length(classic[0]);

        mutable registers = [];
        for i in 0..tupleSize-1 {
            set registers = registers + [i*REGISTER_SIZE..((i+1)*REGISTER_SIZE)-1];
        }

        let init = _initTuple;
        let oracle = _tupleOracle(classic, registers, _, _);
        let size = REGISTER_SIZE * tupleSize + 1;
        let answers = Length(classic);

        log.Info($"Ket Init --> classic: {classic}, registers: {registers}");

        return Ket(init, oracle, size, answers, registers, _validTuple);
    }

    operation Init_1(values: Int[]) : Ket {
        let classic =  Mapped(_t1, values);
        return _init_tuple(classic);
    }

    operation Init_2(values: (Int, Int)[]) : Ket {
        let classic = Mapped(_t2, values);

        return _init_tuple(classic);
    }

    operation Solve(k: Ket) : Ket
    {
        let (originalInit, originalOracle, size, _, r, _) = k!;

        let init = _initSolve(originalInit, _);
        let filter = r[Length(r) - 1];
        let registers = r[0..Length(r) -2];
        let oracle = _solveOracle(originalOracle, filter, _, _);
        // TODO: lazy calculating of estimated answers.
        let answers = _estimateAnswers(init, oracle, size);

        log.Info($"Solve Init --> registers: {registers}, answers:{answers}");

        return Ket(init, oracle, size, answers, registers, _validSolve(k, _));
    }

    operation Prepare(ket: Ket, qubits: Qubit[]) : Unit 
    {
        let (init, oracle, size, answers, _, valid) = ket!;

        repeat {
            ResetAll(qubits);
            init(qubits);
            grover.Apply(oracle, qubits, answers);
        } until (answers == 0 or valid(qubits));
    }


    operation Print(ket: Ket) : Unit 
    {
        let (init, oracle, size,  answers, registers, _) = ket!;

        use qubits = Qubit[size];
        
        // use t = Qubit();
        // ResetAll(qubits);
        // init(qubits);
        // oracle(qubits,t);
        // DumpRegister((), qubits + [t]);
        // Reset(t);
        
        Prepare(ket, qubits);

        Message($"Ket: {qubits}");
        for r in registers {
            log.Info($"  {qubits[r]}");
        }
        DumpRegister((), qubits);

        ResetAll(qubits);
    }

    operation _validTuple(qubits: Qubit[]) : Bool
    {
        let tracker = qubits[Length(qubits) - 1];
        let r = M(tracker);
        log.Debug($"tuple tracker ({tracker}): {r}");
        return r == One;
    }

    operation _validSolve(original: Ket, register: Qubit[]) : Bool
    {
        let (_, _, _, _, registers, o_valid) = original!;

        let qubits = register[0..Length(register) - 2];
        let tracker = register[Length(register) - 1];
        let filter = qubits[registers[Length(registers) - 1]];

        let s = M(tracker);
        log.Info($"  1. solve tracker ({tracker}): {s}");
        if (IsResultOne(s)) {
            let f = All(IsResultOne, ForEach(M, filter));
            log.Info($"  2. filter tracker ({filter}): {f}");
            if (f) {
                if (o_valid(qubits)) {
                    return true;
                }
            }
        }

        return false;
    }
    
    operation _initTuple(qubits: Qubit[]) : Unit
    is Adj {
        ApplyToEachA(H, qubits);
    }

    operation _initSolve(originalInit: Qubit[] => Unit is Adj, qubits: Qubit[]) : Unit
    is Adj {
        originalInit(qubits);
    }

    
    operation _solveOracle(
        oracle: (Qubit[], Qubit) => Unit is Adj + Ctl,
        f: Range,
        qubits: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        use tracker = Qubit();
        let filter = qubits[f];

        log.Debug($"tracker: {tracker}");
        log.Debug($"qubits: {qubits}");
        log.Info($"==> Solve oracle filter:{filter} on:{qubits}");

        within {
            oracle(qubits, tracker);
        } apply {
             Controlled X ([tracker] + filter, target);
        }
    }

    operation _tupleOracle(classic: Int[][], registers: Range[], all: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {

        log.Debug($"all:{all}, target:{target}");

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

    
    operation _estimateWrap(
        init: Qubit[] => Unit is Adj, 
        oracle: (Qubit[], Qubit) => Unit is Adj,
        all: Qubit[]) : Unit is Adj
    {
        let qubits = all[0..Length(all) - 2];
        let target = all[Length(all) - 1];

        log.Info($"estimateWrap qubits:{qubits} target:{target}");
        init(qubits);
        oracle(qubits, target);
    }

    operation _estimateAnswers(
        init: Qubit[] => Unit is Adj, 
        oracle: (Qubit[], Qubit) => Unit is Adj,
        length: Int) : Int
    {
        let identities = [PauliI, size = length];
        let n = IntAsDouble(1 <<< length);
        let f = EstimateFrequencyA(_estimateWrap(init, oracle, _), Measure(identities + [PauliZ], _), length + 1, 1000);
        let t = Round(n * (1.0 - f));
        log.Info($"EstimateFrequencyA: n:{n}, f:{f}, t:{t}");

        return t;
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