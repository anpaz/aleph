
# aleph

This package provides the Python bindings for `aleph`.

`aleph` defines a high level programming model that can be embedded into classical languages to develop large scale quantum hybrid applications, without the quantum mechanics.

## Getting started

To get started, install the aleph-lang package from PyPI:

```bash
pip install aleph-lang
```


## Programming Model

Expressions in Python take a single value, for example in the following program variable $x$ takes value 1:

```python
x = 1
print(x)

# prints: 1
```

`aleph` extends this model by allowing registers to take multiple values simultaneously. This is not achieved by defining a list structure that points to multiple values in memory, instead, all the values are in a single quantum register in *superposition*. We call this new type of register a Ket. 

To construct a Ket in Python, create an instance of the `KetInt` or the `KetBool` class from the `aleph_lang` module. `KetInt` accepts an optional `width` argument to control the width (number of qubits) of the register, it defaults to 3. To read a value from a Ket use the `sample` method.

```python
from aleph_lang import KetInt, sample

random_number = KetInt(width=10)
print(sample(random_number))

#  it prints a number between 0 and 1023 (2^10 - 1).
```

You can perform arithmetic and boolean expressions on Kets. Expression are applied simultaneously to all Ket elements on a single step using *quantum parallelism*. The result is a new Ket containing all possible outcomes of the evaluation.

Input and output Kets of an expression are *entangled*, that is, when sampled the Ket's values are always correlated with each other. This entanglement is preserved across expressions. For example:

```python
x = KetInt()
y = x + 5
z = 2 * y

print(sample([x, y, z]))
# prints [0, 5, 10] or [1, 6, 12] or [2, 7, 14] or ... [7, 12, 24)]
# e.g. the value of the second element is the first plus five, 
# the third element is the second times two.
```

You can filter the elements of a Ket using **where** expressions. For example, use `where_in` to filter the elements of a Ket to a specific list of items:

```python
dice1 = KetInt().where_in(range(1,7))
dice2 = KetInt().where_in(range(1,7))

roll = dice1 + dice2
print(sample([dice1, dice2, roll]))
#prints [0,0,0] or [0,1,1] or [0,2,2] or ... [6,6,12]
```

You can also filter the set of elements `sample` will return passing an optional `when` parameter; when provided, `sample` will return only elements that satisfy the given expression:

```python
# Solve x + a == b * x 
def solve_equation(a, b):
    x = KetInt()
    eq1 = x + a
    eq2 = b * x

    return sample(x, when=(eq1 == eq2))

answer = solve_equation(3, 2)
print(answer)
# prints 3
```

Under the covers, **when** and **where** expressions use *amplitude amplification (a.k.a. Grover)* to amplify the probability of measuring the correct elements and filter out the rest.

`sample` returns a single value; `aleph` also provides `histogram` to get the histogram resulting from the outcomes of sampling Kets multiple times; it takes a `rounds` parameter that indicates how many samples to take:

```python
def dice_roll_histogram():
    dice1 = KetInt().where_in(range(1,7))
    dice2 = KetInt().where_in(range(1,7))

    roll = dice1 + dice2
    return histogram([roll], rounds=1000)
```


It is safe to combine Kets with classical expressions, making it possible to create hybrid programs that leverage the host's language features. For example, the following function takes a graph information and the max number of colors to solve a graph coloring problem:

```python
# Solve a graph coloring problem, for the given number of nodes and list of edges.
def graph_coloring(max_colors, nodes_count, edges):
    def create_node():
        w = width_for(max_colors)
        return KetInt(width=w).where_less_than_equals(max_colors - 1)

    def compare(edges):
        if len(edges) == 1:
            (left, right) = edges[0] 
            return left != right
        else:
            head, *tail = edges
            (left, right) = head
            a = left != right
            b = compare(tail)
            return  a & b

    nodes = [ create_node() for _ in range(nodes_count) ]
    edges = [ (nodes[x], nodes[y]) for (x, y) in edges ]
    filter = compare(edges)

    ## Print to the console a graphviz representation
    ## of the quantum graph:
    ## tree(filter)

    return sample(nodes, when=filter)

max_colors = 3
total_nodes = 4
edges = [ (0, 1), (1, 2), (0, 2), (1, 3) ]
print("graph coloring:", graph_coloring(max_colors, total_nodes, edges))
```


## API server

To simplify its setup `aleph` itself runs on the cloud. It uses an external service that at runtime keeps track of a program's quantum graph and enables its evaluation. No personal information is collected or accessible by the service. To change this behavior, you can run a local instance of the API server and set the `ALEPH_BASE_URL` environment variable to point to your local instance, for example:

```bash
export ALEPH_BASE_URL=http://localhost:7071/
```

Source code for `aleph`'s API server and instructions on how to build and run locally can be found at: https://github.com/anpaz/aleph/tree/main/src/api. 
