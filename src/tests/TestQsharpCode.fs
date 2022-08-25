namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Microsoft.Quantum.Simulation.Simulators
open Microsoft.Quantum.Simulation.Core

open aleph.qsharp
open aleph.runtime.Eval
open aleph.runtime.qpu.qsharp

open aleph.tests.Utils


[<TestClass>]
type TestQsharpCode () =

    let BOOL_REGISTER_SIZE = 1
    let INT_REGISTER_SIZE = 2

    let emptyUniverse = new Universe((0,0, new QArray<QRange>(), null))

    let toQValue = function
        | Bool b -> new QValue((if b then (1,BOOL_REGISTER_SIZE) else (0,BOOL_REGISTER_SIZE)))
        | Int i -> new QValue((i, INT_REGISTER_SIZE))
        | _ -> failwith "not an int/bool"
        
    let toQTuple = function
        | Bool b -> [| (Bool b |> toQValue) |] |> QArray<QValue>
        | Int i -> [| (Int i |> toQValue) |] |> QArray<QValue>
        | Tuple t -> 
            t 
            |> List.map toQValue 
            |> List.toArray
            |> QArray<QValue>
        | _ -> failwith "not a tuple"

    let toQSet = function
        | Set s -> 
            s 
            |> Set.toArray
            |> Array.map toQTuple
            |> QArray<IQArray<QValue>>
        | _ -> failwith "not a set"
        
    let toValue (result: IQArray<QValue>) = 
        let one (v: QValue) =
            if v.size = BOOL_REGISTER_SIZE then
                if v.value = 1 then Bool true else Bool false
            elif v.size = INT_REGISTER_SIZE then
                Int (int v.value)
            else
                failwith "not an int/bool"
        if result.Length = 1 then
            one result.[0]
        else
            Tuple (result |> Seq.map one |> Seq.toList)

    [<TestMethod>]
    member this.TestLiteral () =
        let sim = new QuantumSimulator()

        let test_one (values: Value list, qubits: int) = 
            let v = Set (new Set<Value>(values)) |> toQSet
            let u = ket.Literal.Run(sim, v, emptyUniverse).Result
            printfn "Universe = %A" u
            Assert.AreEqual(int64(values.Length), u.rows)
            Assert.AreEqual(int64(qubits), u.columns)

            let r = Sample.Run(sim, u).Result |> toValue
            printfn "result = %A" r
            Assert.IsTrue(is_valid_answer values r)

        [ 
            [
                Bool true
            ], BOOL_REGISTER_SIZE
            [
                Bool true
                Bool false
            ], BOOL_REGISTER_SIZE
            [
                Int 0
                Int 1
                Int 2
            ], INT_REGISTER_SIZE
            [
                Tuple [ Int 0; Int 0 ]
                Tuple [ Int 0; Int 1 ]
                Tuple [ Int 0; Int 2 ]
            ], INT_REGISTER_SIZE + INT_REGISTER_SIZE 
            [
                Tuple [ Int 0; Bool false; Int 0 ]
                Tuple [ Int 0; Bool true; Int 1 ]
                Tuple [ Int 0; Bool true; Int 2 ]
                Tuple [ Int 2; Bool true; Int 3 ]
            ], INT_REGISTER_SIZE + BOOL_REGISTER_SIZE + INT_REGISTER_SIZE
        ]
        |> List.iter test_one
