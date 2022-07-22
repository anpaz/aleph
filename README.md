# Aleph

Quantum computers open the possibility of solving some of today's most complex problems. They will  be capable to make calculations that our existing computers take hours, days, or even years, in a small fraction of the time.

Their superpower is not that they are faster or smaller. Their advantage comes from the fact that they can leverage quantum mechanics to perform their calculations.

This superpower comes at a price: instead of Boolean tables quantum algorithms use vectors, matrices and complex numbers to describe the evolution of the system.

Existing quantum programming languages do not offer real abstractions to remove this complexity. Even languages that are explicitly designed to be high-level like [Silq](https://silq.ethz.ch/) or [Q#](https://github.com/microsoft/qsharp-language/tree/main/Specifications/Language) are more reminiscent of Verilog than C#, or even C.

Aleph aims to be a true high-level quantum programming language that leverages quantum specific properties and algorithms -like superposition, quantum parallelism, entanglement, amplitude amplification and amplitude estimation- to achieve scale and speed-up. These are implicitly leveraged by the language, though, as such users don't have to deal with quantum mechanics concepts like probabilities, complex numbers or matrices.

## Universes, outcomes, kets

In Aleph all possible outcomes of a single quantum computation are represented as a table, a.k.a quantum universe.

Each column represents a register. A register can hold values of two datatypes: `int` and `bool`. Each row in the universe stands for a single distinct outcome.

> *Note: on a quantum device, each column maps to a single quantum register in super position.*

A quantum universe is manipulated via quantum variables, a.k.a `ket`. A `ket` is a set of registers from a quantum universe.

## Literals

Quantum literals are created between the `|>` symbols. A simple one-column quantum universe can be created by listing all the possible outcomes, for example:

```fsharp
|0>
```

| |
| --- |
| 0 |

creates a one-column, one row universe that when sampled always has outcome 0. Multiple element can be specified using a comma to separate them, for example:

```fsharp
|2,4,6,10>
```

creates a one-column, four rows universe that when sampled can give 2, 4, 6 or 10 as outcome, each one with same probability:

| |
| --- |
| 2 |
| 4 |
| 6 |
| 10 |

Multi-column universes can be created using `tuples`. Each `tuple` represents a row of the universe in which each element represents a value for the corresponding register. For example:

```fsharp
| (0,0,0,0), (0,1,1,0), (1,1,0,0) >
```

Creates a 4 columns, 3 rows universe:

|  |  |  |  |
| --- | --- | --- | --- |
| 0 | 0 | 0 | 0 |
| 0 | 1 | 1 | 0 |
| 1 | 1 | 0 | 0 |

Tuples can mix data-types, however all values on a given column must have the same type. This is a valid literal:

```fsharp
| (0,0,false), (1,1,true) >
```

that creates a 3 columns, 2 rows universe:

|  |  |  |
| --- | --- | --- |
| 0 | 0 | false |
| 1 | 1 | true |

Aleph registers support a special convention for all (`@`):

```fsharp
| @ >
```

An `@` register is one that contains all integer values:

| |
| --- |
| 0 |
| 1 |
| 2 |
| ... |



## Quantum Expressions

Each quantum expression returns a `ket`, i.e. a set of a columns from a quantum universe. Specifically the expression  `| 1, 2, 3, 4 >` creates a one column quantum universe of 4 rows, and returns a `ket` comprised of the first (and only) column.

Similarly, the expression `| (0,0,1), (0,2,0), (3,0,0) >` creates a 3 column universe and returns a `ket` comprised of the corresponding 3 columns.

### Project

> `ket.idx`

Receives a `ket` and an index, and returns a new ket pointing to the column given the corresponding index. For example, in:

```fsharp
let k1 = | (0,0,0), (0,1,1), (1,1,0) > 
let k2 = k1.0
let k3 = k1.1
```

a single universe of 3 columns is created. All `kets` point to this universe, `k1` has a reference to the 3 columns, `k2` has a reference to the first column, and `k1` to the second column 

| k1_0<br>k2_0 | k1_1<br>k3_0 | k1_2<br>- |
| --- | --- | --- |
| 0 | 0 | 0 | 0 |
| 0 | 1 | 1 | 0 |
| 1 | 1 | 0 | 0 |

### Join

> `(k1, k2)`

Join takes two kets, and returns a new ket comprised of the union of the columns in both the arguments.

The universe the results points to depend on the the inputs:

1. If the input `kets` come from the same universe, the result is a ket pointing to this universe.
1. If the input `kets` come from different universes, the result is a new universe corresponding to the cross-product of the input universes, with a one-to-one mapping to the columns of the input universes.

For example, in this program:

```fsharp
let k1 = | (0,0,0), (0,1,1), (1,1,0) > 
let k2 = k1.0
let k3 = k1.1
let k4 = (k2, k3)
```

k2 and k3 are projections from the universe created by k1. As such, k4 which is the Join of k2 and k3 points to the same universe:

| k1_0<br>k2_0<br>-<br>k4_0 | k1_1<br>-<br>k3_0<br>k4_1 | k1_2<br>-<br>-<br>- |
| --- | --- | --- |
| 0 | 0 | 0 | 0 |
| 0 | 1 | 1 | 0 |
| 1 | 1 | 0 | 0 |

On the other hand, in this program:

```fsharp
let k5 = | 0, 1 > 
let k6 = | true, false >
let k7 = (k5, k6)
```

k5 and k6 are two literals that create their own quantum universe, when joined the resulting ket points to a new universe with the cross-product:

| k7_0 | k7_1 |
| --- | --- |
| 0 | true |
| 0 | false |
| 1 | true |
| 1 | false |

### Ket\<int> Expressions

> * Add (`k1 + k2`)
> * Multiply (`k1 * k2`)
> * Equals (`k1 == k2`)

These expressions take two one-column `ket<int>` expressions and add a column to the universe with the result of the corresponding operation. As with `Join`, the resulting universe depends on the input `kets`.

1. If the input `kets` come from the same universe, the result is the same universe with an extra new column with the result of the corresponding operation applied to each row.
1. If the input `kets` come from different universes, they are first joined then an extra column is appended with the result of the corresponding operation applied to each row.

In both scenarios, the result is a ket pointing to the extra column with the result of the operation.

For example, in this program:

```fsharp
let k1 = | (0,0), (0,1), (1,1) > 
let k2 = k1.0
let k3 = k1.1
let k4 = k2 + k3
```

k2 and k3 are projections from the universe created by k1. As such, k4's universe is a new based on k1's universe with an extra column for the result of the operation

| |  | k4_0 |
| --- | --- | --- |
| 0 | 0 | 0 |
| 0 | 1 | 1 |
| 1 | 1 | 2 |

In contrast, in this program:

```fsharp
let k5 = | 0, 1 > 
let k6 = | 0, 1 >
let k7 = k5 + k6
```

k5 and k6 are two literals that create their own quantum universe, when the expression is applied they are joined and a new column with the operation's result is created:

|   |   | k7_0 |
| --- | --- | --- |
| 0 | 0 | 0 |
| 0 | 1 | 1 |
| 1 | 0 | 1 |
| 1 | 1 | 2 |

### Ket\<bool> Expressions

> * And (`k1 and k2`)
> * Or (`k1 or k2`)
> * Not (`not k1`)

These expressions take one or two one-column `ket<bool>` expressions and apply the corresponding boolean expression following the same semantics as `ket<int>` expressions.

> Notice that both int and bool expressions can leverage **quantum parallelism** to apply the result of the operation in **one** step. 
>
> As opposed toa classical computer that would normally have to calculate the result of the expression on each row, a quantum computer is capable of calculating the expression in all rows since they are in **superposition**.

### Solve

> `Solve (ket, bool expression)`

Solve takes a `ket` and a `bool quantum expression` and returns a ket from a universe that is equal to the universe of the input ket, with all the rows filtered to only those that satisfy the boolean expression.

For example:

```fsharp
let k1 = | (0,0,0), (0,0,1), (0,1,0), (0,1,1), (1,0,0), (1,0,1) > 
let k2 = k1.0 + k1.2
let k3 = Solve ( (k1.0, k1.2), k2 == |1>)
```

creates the following universe for k3:

| k1_0<br>k3_0 | k1_1<br>- | k1_2<br>k3_1 | k2_0 | \|1\> | k2 == \|1\> |
| --- | --- | --- | --- | --- | --- |
| 0 | 0 | 1 | 1 | 1 | true |
| 0 | 1 | 1 | 1 | 1 | true |
| 1 | 0 | 0 | 1 | 1 | true |

* k1 is the source for the first 3 columns.
* k2 (adding columns 0 and 2) is the source of the next column.
* the literal `|1>` is the source of the next column
* the `==` in the predicate is the source of the last column

> **Note 1:** to filter such a table on a classical computer requires iterating through all the rows. On a quantum computer the number of operations depend on the number of columns when using **amplitude amplification**.

> **Note 2:** quantum expressions return a `ket` who keeps a reference to its input arguments thus creating a DAG. This DAG defines dependencies of evaluation creating **entanglement**.

### Estimate

> `Estimate (bool expression)`

Estimate returns the percentage of rows in the universe that satisfy the corresponding boolean expression.

For example:

```fsharp
let k1 = | (0,0,0), (0,0,1), (0,1,0), (0,1,1), (1,0,0), (1,0,1) > 
let k2 = k1.0 + k1.2
Estimate ( k2 == |1> )
```

returns 0.5

### Classic & mixed expressions



> **Note:** For datasets with large number of elements, Monte Carlo is a common method to calculate the estimated value of a variable, using **amplitude estimation** it is possible to get a quadratic speed up for the same task.

## Examples

### Coin flip

Aleph's most basic program is a  coin flip:

```aleph
let coin = | 1, 0 >
| coin |
```

The first instruction (`|>`) creates a one-column universe of two values, namely: 

| coin |
|-----|
| 0 |
| 1 |

The second instruction (`||`) samples and returns a value from this quantum universe. In this particular case, since there are only two possible outcomes on the universe (0 or 1), it will return either with the same possibility.


### Rolling two dices

```aleph
let dice1 = | 1,2,3,4,5,6 >
let dice2 = | 1,2,3,4,5,6 >

let roll = (dice1, dice2)
| roll |
```

The first two instructions creates two independent quantum universes, each one comprised of one column with 6 rows.

The next instruction **joins** the kets from these two quantum universes thus creating a new universe comprised of 2 columns and 36 rows:

| roll_0 | roll_1    |
| --- |:--- |
|  1  |  1  |
|  1  |  2  |
|  1  |  3  |
| ... |     |
|  6  |  6  |

On the last instruction, `roll` is sampled and one row from its universe is selected at random and returned as a tuple.


### Solving systems of equaltions

```fsharp
let x = | @ >
let eq1 = 4 * (x * x) + (5 * x) - 3
let eq2 = x + 2
| Prepare (Solve (x, eq1 == eq2)) |
```

The first instruction prepares a register in full super-position. That is, it takes any integer value from 0..2^n.

We then use aleph to Solve for `x` in the case where `eq1 == eq2`. Solve will return the universe's x values where eq1 == eq2. 

### Color graphing

Solves the coloring problem for a graph in which no two adjacent nodes should have the same color.

```fsharp
let RED   = 0
let BLUE  = 1
let GREEN = 2


// A function that return the list of all available colors:
let colors() =
    | RED, BLUE, GREEN >

// Edges are listed classically, so we can iterate through them
let edges = {
  (0, 1),
  (1, 2),
  (3, 1), 
  (2, 0)
}

// checks if the coloring for the nodes x and y is invalid.
// invalid is when the 2 nodes of an edge have the same color.
let is_valid_edge_coloring (color1: ket<int>, color2: ket<int>) =
    color1 != color2

// A valid color combination oracle.
// Returns true only if the nodes' color combination is valid for all edges.
let classify_coloring (edges: set<int, int>, coloring: ket<int, int, int, int>) =
    let valid = summarize e in edges with and
        let (x, y) = e
        is_valid_edge_coloring (coloring.x, coloring.y)
    valid

// A ket with the color combination for all nodes. Each node is an item of a tuple.
let nodes_colors = (colors(), colors(), colors(), colors())

// To find a valid coloring, solve the valid_combination oracle and
// measure the result
let classification = classify_combinations (edges, nodes_colors)
let answers = Solve(edges, classification==true)
| answers |
```

### Numerical Integration

**TODO...***
