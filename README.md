# aleph

## quantum registers

a classical register always yield the same value


one of the key differences of a quantum register, is that it can be prepared to state that yields a random value with certain (complex) probability


unfortunately, once you read the register the state "collapses" to a single value and you loose the rest of the information.


## quantum logic

it would seem it's not possible to perform boolean logic on a quantum register without collapsing it and loosing it's quantum properties; however, a basic instruction on every quantum computer is the NOT gate, which flips the probabilities on the state of a qubit


and a key ability of a quantum computer is the ability to perform "controlled operations" in which the operation is executed iff the state of one or more control qubits is "1"


leveraging this you can emulate an AND gate that flips the target iff both qubits are in state 1


and an OR gate in which the target qubit flips if either of the qubits are in state 1


notice that if the controlled qubits are in superposition, the target qubit will be flipped with only the corresponding probability



## quantum oracles

a quantum oracle is a quantum operation that given a register, flips a target qubit iff the register includes a certain value in its state. it must be reversible, and it can't affect the original state. 

they are widely used in quantum algorithms

* Bernstein-Vazirani
* Deutsch-Jozsa
* Grover search

the trick to implement an oracle that checks if the register is in certain "integer" state, is to flip the 0 bits of the integer, applied a Controlled NOT using the register as control, and undo the flips:

to check if a qubit is in any of $n$ different integers, you must check each value on $n$ temporary qubits and perform an OR of all this temp qubits.


## quantum oracles in aleph

within aleph, a quantum integer register is a built-in datatype: qInt
// Creates a quantum register in superposition with possible values 3,5,15
val r1 = qInt(3, 5, 15)

qInt supports membership expressions with `in`, which generates the corresponding quantum oracle

// x is an oracle to check if 6 is a possible value of r1
val x = 6 in r1

it support boolean expressions on oracles using `and`, `or`, `not`:
val x2 = (6 in r1) and not (0 in r1)

oracle expressions can be built from different registers
val x3 = (6 in r1) or (12 in r2)

aleph provides the `any` and `all` operators as sugar for `or` and `and`:
val x4 = (any(1, 3, 12) in r1) or (all(2,4,6) in r2)

for each oracle, is is possible to:
* evaluate it for testing
* generate a json representation of the corresponding circuit
* generate the Q# code to incorporate it into another quantum program.
