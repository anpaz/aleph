namespace aleph.runtime.qpu.qsharp

open aleph.qsharp

open aleph.parser.ast.typed
open aleph.runtime.Eval

type QUniverse = aleph.qsharp.Universe
type QValue = aleph.qsharp.Value

type Memory = {
    allocations: Map<int, QUniverse>
}

type Simulator() =
    let random = System.Random()
    let mutable max_ket = 0

    let mutable memory = { allocations = Map.empty }

    interface QPU with

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
