
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

Expressions normally take a single value, for example here  variable $x$ takes value 1:

```fsharp
let x = 1
```

`aleph` extends this model by allowing registers to take multiple values simultaneously. This is not achieved by defining a list structure that points to multiple values in memory, instead, all the values are in a single quantum register in *superposition*. We call this new type of register a Ket. 

To construct a Ket in F#, use `ket` from the `aleph.kets` module passing the width as argument. `width` controls the number of qubits of the register.

To read a value from a Ket it must be sampled. `aleph` currently provides two different backends to evaluate Kets:
    * **qsharp**: to evaluate `aleph` Kets using Q#'s quantum simulator
    * **classic**: to evaluate `aleph` Kets using in-memory classic semantics

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
#prints [0,0,0] or [0,1,1] or [0,2,2] or ... [6,6,12]
```

You can also filter the set of elements `sample` by using the `sample_when` function; it will return only elements from the Universe that satisfy the given expression:

```fsharp
// Solve x + a == b * x 
let solve_equation a b =
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
printfn "%A" (histogram [ roll ])
```



## API server

To simplify its setup `aleph` itself runs on the cloud. It uses an external service that at runtime keeps track of a program's quantum graph and enables its evaluation. No personal information is collected or accessible by the service. To change this behavior, you can run a local instance of the API server and set the `ALEPH_BASE_URL` environment variable to point to your local instance, for example:

```
export ALEPH_BASE_URL=http://localhost:7071/
```

Source code for `aleph`'s API server and instructions on how to build and run locally can be found at: https://github.com/anpaz/aleph/tree/main/src/api. 
