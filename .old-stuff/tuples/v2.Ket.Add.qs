namespace ket.v2 {
    open Microsoft.Quantum.Intrinsic;


    operation Add(ket: Ket) : Ket {
        let (_prepare, _size, _registers) = ket!;

        let left = _registers[0];
        let right = _registers[1];
        let answer = _size .. _size+1;

        let prepare = _Add_prepare(left, right, answer, _prepare, _);
        let size = _size + 2;
        let registers = [left, right, answer];
        
        return Ket(prepare, size, registers);
    }



    operation _Add_prepare(l: Range, r: Range, a: Range, _prepare: Qubit[] => Unit, memory: Qubit[]) : Unit
    {
        _prepare(memory);
        
        let answer = memory[a];
        let x = memory[l];
        let y = memory[r];

        //TODO: let l = Length(x);
        CNOT(x[0], answer[0]);
        CNOT(y[0], answer[0]);
        CCNOT(x[0], y[0], answer[1]);

        CNOT(x[1], answer[1]);
        CNOT(y[1], answer[1]);
    }


}