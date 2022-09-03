namespace ket.v2 {

    open Microsoft.Quantum.Diagnostics;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Arrays;


    operation _Tuple(classic: Int[][]) : Ket
    {
        let REGISTER_SIZE=2;
        let tupleSize = Length(classic[0]);

        mutable registers = [];
        for i in 0..tupleSize-1 {
            set registers = registers + [(i*REGISTER_SIZE)..((i+1)*REGISTER_SIZE)-1];
        }

        //let init = _Tuple_init;
        //let oracle = _Tuple_oracle(classic, registers, _, _);
        let size = REGISTER_SIZE * tupleSize;
        let answers = Length(classic);
        //let isValid = _Tuple_isValid(tracker, _);

        let prepare = _Tuple_prepare(classic, registers, answers, _);

        log.Info($"Ket Init --> classic: {classic}, registers: {registers}");

        //return Ket(init, oracle, size, answers, registers, tracker, isValid);
        return Ket(prepare, size, registers);
    }

    operation Tuple_1(values: Int[]) : Ket {
        let classic =  Mapped(_t1, values);
        return _Tuple(classic);
    }

    operation Tuple_2(values: (Int, Int)[]) : Ket {
        let classic = Mapped(_t2, values);

        return _Tuple(classic);
    }

    operation _Tuple_prepare(classic: Int[][], registers: Range[], answers: Int, memory: Qubit[]) : Unit 
    {
        mutable qubits = [];
        for r in registers {
            set qubits = qubits + memory[r];
        }

        use tracker = Qubit();
        set qubits = qubits + [tracker];

        repeat {
            _Tuple_init(qubits);
            let oracle = _Tuple_oracle(classic, registers, _, _);
            grover.Apply(oracle, qubits, answers);
            //DumpRegister((), qubits);
        } until (_Tuple_isValid(tracker) or answers == 0);

        //DumpRegister((), qubits);
    }

    operation _Tuple_init(qubits: Qubit[]) : Unit
    {
        ResetAll(qubits);
        ApplyToEachA(H, qubits);
    }

    operation _Tuple_oracle(classic: Int[][], registers: Range[], all: Qubit[], target: Qubit) : Unit
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

    operation _Tuple_isValid(tracker: Qubit) : Bool
    {
        let r = M(tracker);
        log.Info($"  * tuple tracker ({tracker}): {r}");
        return r == One;
    }

    function _t1<'T>(v1: 'T) : 'T[] {        
        return [v1];
    }

    function _t2<'T>(values: ('T, 'T)) : 'T[] {        
        let (v1, v2) = values;
        return [v1, v2];
    }
}