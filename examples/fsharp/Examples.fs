module Examples

open aleph.kets
open context

// Randomly get a 0 or 1 value
let coin_flip() =
    let coin = ket 1
    sample [coin]

// Get a random number    
let random_number() =
    let random = ket 10
    sample [random]

// Roll to dices and return the sum
let dice_roll() =
    let dice1 = (ket 3).Where(In [1..6])
    let dice2 = (ket 3).Where(In [1..6])

    let roll = dice1.Add(dice2)
    sample [ dice1; dice2; roll ]
    

// Roll to dices and return the histogram of the sum
let dice_roll_histogram() =
    let dice1 = (ket 3).Where(In [1..6])
    let dice2 = (ket 3).Where(In [1..6])

    let roll = dice1.Add(dice2)
    histogram ([ roll ], 1000)

// Solve x + 3 == 2x 
let solve_equation() =
    let x = ket 3
    let eq1 = x.Add(3)
    let eq2 = x.Multiply(2)
    
    sample_when ([x; eq1; eq2], eq1.Equals(eq2))

// Solve a graph coloring problem, for the given number of nodes and list of edges.
let solve_graph_coloring (max_colors: int) (nodes_count: int) (edges: (int * int) list)  =
    let create_node _ = 
        let w = aleph.utils.int_width (max_colors - 1)
        (ket w).Where(LessThanEquals, max_colors - 1)

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

// Calls graph coloring with a tiny (2 color) graph
let tiny_graph_coloring() = 
    let max_colors = 2
    let nodes_count = 4
    let edges = [ (0, 1); (0, 2); (0, 3) ]
    solve_graph_coloring max_colors nodes_count edges

// Calls graph coloring with a small (3 color) graph
let small_graph_coloring() = 
    let max_colors = 3
    let nodes_count = 4
    let edges = [ (0, 1); (1, 2); (0, 2); (1, 3) ]
    solve_graph_coloring max_colors nodes_count edges
