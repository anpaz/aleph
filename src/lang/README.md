
# aleph

This package provides the F# integration of `aleph`.

`aleph` defines a high level programming model that can be embedded into classical languages to develop large scale quantum hybrid applications, without the quantum mechanics.

## Getting started

Create a new F# project and install the `aleph.lang` package.

```
dotnet new console -lang f#
dotnet add package aleph.lang 
```

## Programming Model

Expressions normally take a single value, for example here  variable *x* takes value 1:

```fsharp
let x = 1
```

`aleph` extends this model by allowing registers to take multiple values simultaneously. This is not achieved by defining a list structure that points to multiple values in memory, instead, all the values are in a single quantum register in *superposition*. We call this new type of register a Ket. 

To construct a Ket in F#, use `ket` from the `aleph.kets` module passing the width as argument. `width` controls the number of qubits of the register.

To read a value from a Ket it must be sampled. `aleph` currently provides two different backends to evaluate Kets:
    - **qsharp**: to evaluate `aleph` Kets using Q#'s quantum simulator
    - **classic**: to evaluate `aleph` Kets using in-memory classic semantics

Select the context you want to program to run to import the corresponding `sample` function:

```fsharp
open aleph.kets
open aleph.qpu.classic.context

let coin = ket 1
let value = sample [coin]

printfn "%A" value
// prints 0 or 1, with the same probability
```

You can perform arithmetic and boolean expressions on Kets. Expression are applied simultaneously to all Ket elements on a single step using *quantum parallelism*. The result is a new Ket containing all possible outcomes of the evaluation.

Input and output Kets of an expression are *entangled*, that is, when sampled the Ket's values are always correlated with each other. This entanglement is preserved across expressions. For example:

```fsharp
let x = ket 4
let y = x.Add(5)
let z = y.Multiply(2)

printfn "%A" (sample [x; y; z])
// prints [0, 5, 10] or [1, 6, 12] or [2, 7, 14] or ... [15, 20, 40)]
// e.g. the value of the second element is the first plus five, 
// the third element is the second times two.
```

You can filter the elements of a Ket using **where** expressions. For example, to filter the elements of a Ket to a specific list of items:

```fsharp
let dice1 = (ket 3).Where(In [1..6])
let dice2 = (ket 3).Where(In [1..6])

let roll = dice1.Add(dice2)
printfn "%A" (sample [ dice1; dice2; roll ])
// prints [0,0,0] or [0,1,1] or [0,2,2] or ... [6,6,12]
```

You can also filter the set of elements `sample` by using the `sample_when` function; it will return only elements from the Universe that satisfy the given expression:

```fsharp
// Solve x + a == b * x 
let solve_equation (a: int) (b: int) =
    let x = ket 3
    let eq1 = x.Add(a)
    let eq2 = x.Multiply(b)
    
    sample_when ([x], eq1.Equals(eq2))

printfn "%A" (solve_equation 3 2)
// prints 3
```

Under the covers, **when** and **where** expressions use *amplitude amplification (a.k.a. Grover)* to amplify the probability of measuring the correct elements and filter out the rest.

`sample` returns a single value; `aleph` also provides `histogram` to get the histogram resulting from the outcomes of sampling Kets multiple times; it takes a `rounds` parameter that indicates how many samples to take:

```fsharp
let dice1 = (ket 3).Where(In [1..6])
let dice2 = (ket 3).Where(In [1..6])

let roll = dice1.Add(dice2)
printfn "%A" (histogram ([ roll ], 1000))
```

It is safe to combine Kets with classical expressions, making it possible to create hybrid programs that leverage the host's language features. For example, the following function takes a graph information and the max number of colors to solve a graph coloring problem:

```fsharp
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



let max_colors = 3
let nodes_count = 4
let edges = [ (0, 1); (1, 2); (0, 2); (1, 3) ]

let answer = solve_graph_coloring max_colors nodes_count edges
printfn "%A" answer
```
