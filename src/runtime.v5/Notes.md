

# Tuples:
Tuples are always flat list of literals. Constructing Tuples from other other tuples flattens them.
// ((3)) --> (3)
// ((0,1)) --> (0,1)
// ((0,1), 2, ((3, 4), 5)) --> (0,1,2,3,4,5)

This is because ((,),) is a special case of the join operation:

* () -> ()
* (1, 2) -> (1,2)
* (()) -> ()
* ((1,2), ()) -> (1,2)
* ((1,2),3) = (1,(2,3)) = (1,2,3)
* ((1,2),(3,4)) = (1,2,3,4)

when applied to sets, join creates the cross-product:
* ([a, b], c) -> [(a,c), (b,c)]
* (a, [b, c]) -> [(a,b), (a,c)]
* ([a,b], [c,d]) -> [(a,c), (a,d), (b,c), (b,d)]
* ([a,b], (c,d)) -> ([a,b], [(c,d)]) -> [(a,(c,d)), (b,(c,d))] [(a,c,d), (b,c,d)]


## Expressions:

* Project:  (1,2,3,4).0 = 1
            (1,2,3,4).[0,2] = (1,3)
* Add:       (1,2) + (3,4) = (4, 6)
* Substract: (1,2) - (3,4) = (-2, -2)
* Multiply: (1,2) * (3,4) = (3, 8)

    > notes: 
    > * modular arithmetic, based on the size or the biggest register
    > * each tuple must have the same dimension
    > * boolean are treated like integers of register size 1

# Sets
By definition, sets have unique values:
// [ t1; t1 ] --> [ t1 ]

As tuples, Sets are flat lists. Constructing sets from sets flattens them:
// [ [(0,0) (1,1)], (0,1), (1,1)  ] --> [ (0,0), (0,1), (1,1) ]

All tuples in a set must have the same type, trying to create a set
with tuples of different types causes an error

## Expressions:

* Add:          [(1,2), (3,4)] + [(1,2), (5,6)] = [(1,2), (3,4), (5,6)]
* Substract:    [(1,2), (3,4)] - [(1,2), (5,6)] = [(3,4)]
* Project:      [(1,2), (3,4)].0 = [1, 3]
                [(1,2,3,4), (5,6,7,8)].[0,2] = [(1,5), (3,7)]

// TODO
* Intersection: [(1,2), (3,4)] intersect [(1,2), (5,6)] = [(1,2)]

# Kets

Kets are quantum variables.

During classical evaluation a Ket represents a set of quantum registers *and*
its state preparation quantum expression. The state preparation can depend on other
kets, defining a DAG.

A classical value can be obtained from a Ket only by sampling it. Sampling consists
of doing the quantum evaluation of the Ket's state preparation.

The quantum evaluation consists of recursively applying the state preparation 
expressions of the kets in the DAG.




The type of a Ket
is the union of the type of its registers.

> Note: 
>        QInt == Ket<Int>
>        QBool == Ket<Bool>


## Expressions

Quantum expressions always take Kets as input and return a Ket. On mixed expressions,
the classical elements are first converted into their Ket representation, e.g.:

```
3 + |1,2> == |3> + |1, 2>
```



> **Background, implementation details**
> 
>     All Ket expressions need to happen on a single Ket. Even more, the input
>     and output needs to remain on the same Ket as the registers normally end up 
>     entangled.
> 
>     To achieve this, when adding to registers the compiler first joins them
>     into a new ket, the result is then appended to this Ket and it returns 
>     the projection with the result:
>     For example, to add quantum registers `|0, 1>` and `|2, 4>`:
> 
>     1. Join to `|(0,2), (0,4), (1,2), (1,4)>`
>     2. Add a new register with the result of adding the first two:
>     `|(0,2,2), (0,4,4), (1,2,3), (1,4,5)>`
>     3. Projects the third register
>     `|(0,2,*2*), (0,4,*4*), (1,2,*3*), (1,4,*5*)>`
> 
>     Notice that the projection doesn't destroy or modifies the Ket's content, it only
>     changes what registers are externally visible.
> 
>     This implies that the Ket's type only reflects the externally accesible registers,
>     but internally it may contains a lot more.


### Literals

Literal Kets are built from a classical value. Their type mimics the one of the 
classical value. e.g.

```
Q.Literal Int ==> Ket<Int>
Q.Literal Set<Int,Bool,Bool> ==> Ket<Int,Bool,Bool>
```

## Projection

Project takes a Ket and a list of indices, and returns a new Ket with only the
corresponding registers.

```
Project Ket<Int,Bool,Int> [0,1] --> Ket<Int,Bool>
```

## And, Or

Take a Ket<Bool, Bool> pair, and return a Ket<Bool>

## Equals

Take a QInt pair, and return a QBool

## +, * -

Take a QInt pair, and return a QInt

## Sample

Takes a Ket and returns a classical value that mimics the Ket's type.

## Measure

Takes a Ket and a number of shots and returns a histogram in which the keys
mimics the Ket's type, and the values are integers (representing the number
of times that value was measured).

## Methods

Methods can take classical and ket arguments. Their type depends on the return value type.

More over, (once we do argument's type inference) the same method
can be used to calculate both, classical and ket values.

For example take the `line` method:

```
let line m x b =
    let s0 = m * x
    let s1 = s0 + b
    s1
```

If we call `line`  with only int values, it returns an integer, e.g.:

```
let y = line 1 2 3      // y == Int
```

but by changing any of the arguments to be a Ket, it becomes a quantum operation:

```
let y line |0,1,2> 2 3          // y == Ket<Int>
```

To see why, if `m` is a `Ket` then
```
let line m x b =
    let s0 = m * x              // Ket * int == Ket
    let s1 = s0 + b             // Ket + int == Ket
    s1
```

## A Note on Expressions

All quantum expressions take a Ket and return a Ket. For example, Q.Add
takes a `Ket<Int, Int>` and returns a `Ket<Int>`. Obviously, the values in the
returned Ket depend on the input, so for example if we have:
```
k1 = | (0,0), (0,1), (1,0), (1,1) >
k2 = Q.Add (k1)
```

then k2's histogram is:
    * 0: .25
    * 1: .50
    * 2: .25

more over, k2 and k1 are entangled, which means that if they're joined,
the values of k2 remember where they are coming from, so if:
k3 = Q.Join (k1, k2)

then, k3's hisogram is:
    * (0,0,0) : 0.25
    * (0,1,1) : 0.25
    * (1,0,1) : 0.25
    * (1,1,2) : 0.25

this is different from joining un-entangled kets, as that is normally the cross-product.
As such, if we have:
```
k4 = | (0,0), (0,1) >
k5 = Q.Join (k4, k2)
```

then k5's histogram is:
   * (0,0,0): 0.125
   * (0,0,1): 0.25
   * (0,0,2): 0.125
   * (0,1,0): 0.125
   * (0,1,1): 0.25
   * (0,1,2): 0.125

FAQ: 
- what happens if you modify entangled Kets?
  *This is not possible, as all Kets are read-only*

- but what about destructive actions like measurement?
  *In Aleph, measuring or sampling are non-destructive actions, so applying them to a Ket doesn't modify it*

- is entanglement transitive?
  *Yes*. For example:
```
    k1 = |0,1>
    k2 = |0,1>
    k3 = Q.Join(k1, k2)
    k4 = Q.Add k3
    k5 = Q.Join (k1, k4)
```
    k5's histogram:
    * (0, 0) : 0.25
    * (0, 1) : 0.25
    * (1, 1) : 0.25
    * (1, 2) : 0.25



## Math model

A quantum program is represented as a dag, in which each node represents a quantum expression, and edges connect an expression with its inputs.

The value of a node is selected from a set of integers. 
The set and its dimension depends on the input value. 
The value of the node is randomly selected from the corresponding set, each element with the same probability, specifically:

Let $S_{k | i}$ be the output set of node $k$ given input $i$; let  $m = | S_{k | i} |$ be the dimension of set $S_{k | i}$, then $\forall x \in S_{k | i}, P_k(x | i) = 1 / m$.

The probability of sampling value $x$ from node $k$ is then given by:

$ P_k(x) = \sum_i P_k(x|i) $


A ket, is a subset of nodes.

Kets are entangled when they are connected.

Projecting a ket creates a new ket that is a subset of the original one.

Joining kets creates a new ket from the union.

Sampling a ket consists on evaluating each node by traversing the dag. Each node is evaluated only once.

Given Ket $K = \{ k_a, k_b \}$

$P(x) = \prod_ $

$P(a, b) = P_{k_a}(a) P_{k_b}( b | a )$



# Universe with Oracles

```
let x = |(0,0), (0,1), (1,1)>
let a = x.1 + 1
let b = Filter(x, a = 0)
|b|
```

The expected state of the system change on each statement as follows:

`let x = |(0,0), (0,1), (1,1)>`

| x.0 | x.1 | Prob |
| --- | --- | --- |
| 0 | 0 | *** (33%) |
| 0 | 1 | *** |
| 1 | 0 | |
| 1 | 1 | *** |


`let a = x.1 + 1`

| x.0 | x.1 | a | Prob |
| --- | --- | --- | --- |
| 0 | 0 | 0 | |
| 0 | 0 | 1 | *** (33%) |
| 0 | 1 | 0 | *** |
| 0 | 1 | 1 | |
| 1 | 0 | 0 | |
| 1 | 0 | 1 | |
| 1 | 1 | 0 | *** |
| 1 | 1 | 1 | |

`let b = Filter(x, a = 0)`

| x.0 | x.1 | a | Prob |
| --- | --- | --- | --- |
| 0 | 0 | 0 | |
| 0 | 0 | 1 | |
| 0 | 1 | 0 | ***** (50%) |
| 0 | 1 | 1 | |
| 1 | 0 | 0 | |
| 1 | 0 | 1 | |
| 1 | 1 | 0 | ***** |
| 1 | 1 | 1 | |

It might be possible, but it's not trivial to write Unitary operations that achieves each of these transformations.

Instead, we use Grover to amplify the probability of the final states we would like to be measured.

Grover works by starting on a state in which all output states (i.e. the value of any output register)
have the same probability to be measured and accepting an oracle that sets a bit to 1 iff
the probability of the corresponding state should be amplified.

So starting with the state:

| x.0 | x.1 | Probability |
| --- | --- | --- |
| 0 | 0 | * |
| 0 | 1 | * |
| 1 | 0 | * |
| 1 | 1 | * |

We have to create an oracle `T` that returns 1 if the corresponding state is part of the tuple, in
our example:

| x.0 | x.1 | `T` |
| --- | --- | --- |
| 0 | 0 | 1 |
| 0 | 1 | 1 |
| 1 | 0 |   |
| 1 | 1 | 1 |


Result of expressions are applied to new quantum registers. Because the input state is in full super-position, the result of the expression is calculated in parallel for all possible values.   

| x.0 | x.1 | a | 
| --- | --- |:---:|
| 0 | 0 | 1 |
| 0 | 1 | 0 |
| 1 | 0 | 1 |
| 1 | 1 | 0 |


We can know create another oracle `A`, that returns 1 if the filtering condition a=0


| x.0 | x.1 | a | `A` |
| --- | --- |:---:|---|
| 0 | 0 | 1 |   |
| 0 | 1 | 0 | 1 |
| 1 | 0 | 1 |   |
| 1 | 1 | 0 | 1 |


Conjoining both `T` and `A` achieves the final oracle.


| x.0 | x.1 | a | `T` | `A` | Oralce |
| --- | --- |:---:|---|-----| ------ |
| 0 | 0 | 1 | 1 |   |   |
| 0 | 1 | 0 | 1 | 1 | 1 |
| 1 | 0 | 1 |   |   |   |
| 1 | 1 | 0 | 1 | 1 | 1 |


## Including expression values in outcome

We mentioned that for Grover to work, it needs all the output states to start on a full 
super-position state (i.e. any state has the same probability to be measured).

Consider a program in which we want to sample the value of an expression, for example:

```
let x = |(0,0), (0,1), (1,1)>
let a = x.1 + 1
let b = (x,a)
|b|
```

Assuming we procede as before and start on a state in which only the input
variables are set in super-position. Consider the state after applying expression `a`

| x.0 | x.1 | a | 
| --- | --- |:---:|
| 0 | 0 | 1 |
| 0 | 1 | 0 |
| 1 | 0 | 1 |
| 1 | 1 | 0 |

The 

| x.0 | x.1 | a | Prob |
| --- | --- | --- | --- |
| 0 | 0 | 0 | |
| 0 | 0 | 1 | *** |
| 0 | 1 | 0 | *** |
| 0 | 1 | 1 | |
| 1 | 0 | 0 | |
| 1 | 0 | 1 | *** |
| 1 | 1 | 0 | *** |
| 1 | 1 | 1 | |

Trying to include `a` in the grover filter will fail, since `a` is not in full superposition. Instead, what aleph does is to add extra registers for any expression that needs to be in 


| o | x.0 | x.1 | a | Prob |
| --- | --- | --- | --- | --- |
| 0 | 0 | 0 | 0 | |
| 0 | 0 | 0 | 1 | *** |
| 0 | 0 | 1 | 0 | *** |
| 0 | 0 | 1 | 1 | |
| 0 | 1 | 0 | 0 | |
| 0 | 1 | 0 | 1 | *** |
| 0 | 1 | 1 | 0 | *** |
| 0 | 1 | 1 | 1 | |
| 1 | 0 | 0 | 0 | |
| 1 | 0 | 0 | 1 | *** |
| 1 | 0 | 1 | 0 | *** |
| 1 | 0 | 1 | 1 | |
| 1 | 1 | 0 | 0 | |
| 1 | 1 | 0 | 1 | *** |
| 1 | 1 | 1 | 0 | *** |
| 1 | 1 | 1 | 1 | |

We then start by putting `o` and `x` in full superposition, and then add an oracle `O` in which the row is included only if `o` value matches the value of the expression:

| o | x.0 | x.1 | Prob |
| --- | --- | --- | --- |
| 0 | 0 | 0 | * |
| 0 | 0 | 1 | * |
| 0 | 1 | 0 | * |
| 0 | 1 | 1 | * |
| 1 | 0 | 0 | * |
| 1 | 0 | 1 | * |
| 1 | 1 | 0 | * |
| 1 | 1 | 1 | * |


| o | x.0 | x.1 | a = x.1 + 1 | `T` | `O` | Oracle |
| --- | --- | --- | --- | --- | --- | ---|
| 0 | 0 | 0 | 1 | 1 |   |   |
| 0 | 0 | 1 | 0 | 1 | 1 | 1 |
| 0 | 1 | 0 | 1 |   |   |   |
| 0 | 1 | 1 | 0 | 1 | 1 | 1 |
| 1 | 0 | 0 | 1 | 1 | 1 | 1 |
| 1 | 0 | 1 | 0 | 1 |   |   |
| 1 | 1 | 0 | 1 |   | 1 |   |
| 1 | 1 | 1 | 0 | 1 |   |   |

When sampled, only the correct 3 rows will be returned.