

# Tuples:
Tuples are the basic data-type. Every literal is a 1-item tuple.
1 == (1)

Tuples are always flat list of literals. Constructing Tuples from other other tuples flattens them.
// ((3)) --> (3)
// ((0,1)) --> (0,1)
// ((0,1), 2, ((3, 4), 5)) --> (0,1,2,3,4,5)

Passing a set to a tuple is an error:
// ([3,4]) --> Error

# Sets
Integers and booleans are

Since every literal is a Tuple, even simple sets are transformed into a list of tuples with dimension one.
[1, 3, 4] == [(1), (3), (4)]

All tuples in a set must have the same dimension, trying to create a tuple
with tuples of different dimension can 
