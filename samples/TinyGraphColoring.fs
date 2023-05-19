module TinyGraphColoring

open aleph.kets
open context


let program() =
    let create_node _ = ket 1

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

    sample_when (nodes, filter)
