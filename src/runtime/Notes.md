

# Tuples:
Tuples are the basic data-type. Every literal is a 1-item tuple.
1 == (1)

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
* ([a,b], [c,d]) -> [(a,c,d), (b,c,d)]


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
Since every literal is a Tuple, even simple sets are transformed into a list of tuples with dimension one.
[1, 3, 4] == [(1), (3), (4)]

By definition, sets have unique values:
// [ t1; t1 ] --> [ t1 ]

As tuples, Sets are flat lists. Constructing sets from sets flattens them:
// [ [(0,0) (1,1)], (0,1), (1,1)  ] --> [ (0,0), (0,1), (1,1) ]

All tuples in a set must have the same dimension, trying to create a tuple
with tuples of different dimension causes an error

## Expressions:

* Add:          [(1,2), (3,4)] + [(1,2), (5,6)] = [(1,2), (3,4), (5,6)]
* Substract:    [(1,2), (3,4)] - [(1,2), (5,6)] = [(3,4)]
* Intersection: [(1,2), (3,4)] intersect [(1,2), (5,6)] = [(1,2)]
* Project:      [(1,2), (3,4)].0 = [1, 3]
                [(1,2,3,4), (5,6,7,8)].[0,2] = [(1,5), (3,7)]

# Kets

Kets take the same argument as a set, and builds a quantum variable for it.
They follow the same constraints.

Ket's can't take other Kets as parameters.
