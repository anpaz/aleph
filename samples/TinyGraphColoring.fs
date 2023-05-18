module TinyGraphColoring

open aleph.kets
open aleph.qpu.classic.context


let program =
    // Return the list of all available colors (only 2):
    let create_node _ = Ket(Literal (width=1))

    let rec compare (edges: (Ket * Ket) list) =
        match edges with
        | [] -> Ket (Constant 1)
        | [ one ] ->
            let left, right = one
            left.Equals(right).Not()
        | head::tail ->
            let left, right = head
            let a = left.Equals(right).Not()
            let b = compare tail
            a.And(b)


    let nodes =  [1..4] |> List.map create_node
    let edges = [
        (nodes.[0], nodes.[1])
        (nodes.[0], nodes.[2])
        (nodes.[0], nodes.[3])
    ]

    let filter = compare edges

    nodes, filter |> Some
