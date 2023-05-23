module GraphColoring

open aleph.utils
open aleph.kets

open context

let graph_coloring (max_colors: int) (nodes_count: int) (edges: (int * int) list)  =
    let create_node _ = 
        let w = int_width (max_colors - 1)
        KetValue(Literal (width=w)).Where(LessThanEquals, max_colors - 1)

    let compare_all_edges (edges: (KetValue * KetValue) list) =
        let isValid (edge: KetValue * KetValue) =
            let left, right = edge
            left.Equals(right).Not()

        let join (before: KetValue) edge =
            before.And(isValid edge)
        
        edges.Tail |> List.fold join (isValid edges.Head)

    let nodes =  [1..nodes_count] |> List.map create_node
    let edges = edges |> List.map (fun (x, y) -> (nodes.[x], nodes.[y]))
    let filter = edges |> compare_all_edges

    sample_when (nodes, filter)

let program() =
    let max_colors = 3
    let total_nodes = 4
    let edges = [
        (0, 1)
        (1, 2)
        (0, 2)
        (1, 3)
    ]
    
    graph_coloring max_colors total_nodes edges