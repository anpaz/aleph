namespace aleph.runtime.qpu.qsharp

open aleph.parser.ast.typed
open aleph.runtime.Eval


type Memory = {
    allocations: Map<int, int list>
    state: Value list list
}

type Ket(id: int, statePrep: Q) =
    interface IKet with
        member this.CompareTo(obj: obj): int = 
            failwith "Not Implemented"
    member this.Id = id
    member this.StatePrep = statePrep

type QSharp() =
    let random = System.Random()
    let mutable max_ket = 0

    let mutable memory = { allocations = Map.empty; state = [] }

    interface QPU with

        member this.Assign (q, ctx) =
            assert (ctx.qpu = this)
            max_ket <- max_ket + 1
            (Value.Ket (new Ket(max_ket, q)), ctx) |> Ok

        (*
            Measure works by randomly picking a row from the universe with the same probability
            from the universe, and then projecting (selecting) only the columns
            associated with the ket.
            Once measured, the universe is collapsed to this value, and next time it is measured
            it will return the same value.
         *)
        member this.Measure (universe: IUniverse) =
            "Measure Not implemented" |> Error
        
        member this.Prepare (u, ctx) =
            "Prepare Not implemented" |> Error
