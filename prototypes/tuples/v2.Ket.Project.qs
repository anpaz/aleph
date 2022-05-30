namespace ket.v2 {

    operation Project(ket: Ket, idx: Int[]) : Ket {
        let (prepare, size, _registers) = ket!;

        mutable registers = [];
        for i in idx {
            set registers = registers + [_registers[i]];
        }

        return Ket(prepare, size, registers);
   }
}