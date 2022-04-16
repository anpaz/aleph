namespace ket {

    open Microsoft.Quantum.Measurement;
    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Characterization;
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Arrays;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Diagnostics;


    operation Solve(k: Ket) : Ket
    {
        let (init, oldOracle, size, _, oldRegisters, tracker, oldValid) = k!;
        let r = Length(oldRegisters);

        let filter = oldRegisters[r - 1];
        let registers = oldRegisters[0..r-2];
        let oracle = _Solve_oracle(oldOracle, filter, _, _);
        // TODO: lazy calculating of estimated answers.
        let answers = _estimateAnswers(init, oracle, size);
        let isValid = _Solve_isValid(oldValid, filter, _);

        log.Info($"Solve Init --> registers: {registers}, answers:{answers}");

        return Ket(init, oracle, size, answers, registers, tracker, isValid);
    }

    operation _Solve_init(originalInit: Qubit[] => Unit is Adj, qubits: Qubit[]) : Unit
    is Adj {
        originalInit(qubits);
    }


    operation _Solve_oracle(
        oracle: (Qubit[], Qubit) => Unit is Adj + Ctl,
        f: Range,
        qubits: Qubit[], target: Qubit) : Unit
    is Adj + Ctl {
        use tracker = Qubit();
        let filter = qubits[f];

        log.Debug($"tracker: {tracker}");
        log.Debug($"qubits: {qubits}");
        log.Debug($"==> Solve oracle filter:{filter} on:{qubits}");

        within {
            oracle(qubits, tracker);
        } apply {
             Controlled X ([tracker] + filter, target);
        }
    }

    operation _Solve_isValid(original: (Qubit[] => Bool), r: Range, qubits: Qubit[]) : Bool
    {
        if (original(qubits)) {
            let filter = qubits[r];
            let f = All(IsResultOne, ForEach(M, filter));
            log.Info($"  * solver filter ({filter}): {f}");
            if (f) {
                return true;
            }
        }

        return false;
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


    operation _estimateWrap(
        init: Qubit[] => Unit is Adj, 
        oracle: (Qubit[], Qubit) => Unit is Adj,
        all: Qubit[]) : Unit is Adj
    {
        let qubits = all[0..Length(all) - 2];
        let target = all[Length(all) - 1];

        log.Debug($"estimateWrap qubits:{qubits} target:{target}");
        init(qubits);
        oracle(qubits, target);
    }
}