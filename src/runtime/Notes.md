

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
Each Ket represents a set of quantum registers. The type of a Ket
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

## And, Or, Equal

Take a QBool pair, and return a QBool

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
