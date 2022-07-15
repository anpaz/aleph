namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.parser.ast
open aleph.parser.ast.typed
open aleph.runtime.Eval


module ClassicValueContext =
    let ctx =
        {
            qpu = { new QPU with
                member this.Assign(arg1: Q): Ket = failwith "Not Implemented"
                member this.Measure(arg1: Ket): Value = failwith "Not Implemented"
                member this.Prepare(arg1: Ket, arg2: ValueContext): Result<(Ket * ValueContext),string> =  failwith "Not Implemented"
                member this.Reset(): unit =  failwith "Not Implemented" 
            }
            heap =  Map [
                "i1", Int 1
                "b1", Bool true
                "t1", Tuple [Bool false; Int 1]
                "t2", Tuple [Bool true; Int 2]
                "s1", Set (Set.ofList  [
                    Tuple [Bool false; Int 0]
                    Tuple [Bool false; Int 1]
                    Tuple [Bool false; Int 2]
                ])
            ]
            types = aleph.parser.TypeChecker.TypeContext [ 
                "i1", AnyType.Type Type.Int
                "b1", AnyType.Type Type.Bool
                "t1", AnyType.Type (Type.Tuple [Type.Bool; Type.Int])
                "t2", AnyType.Type (Type.Tuple [Type.Bool; Type.Int])
                "s1", AnyType.Type (Type.Set (Type.Tuple [Type.Bool; Type.Int]))
            ]
        }

[<TestClass>]
type TestEvalClassic () =

    member this.TestExpression ctx (e, v)=
        match run (e, ctx) with
        | Ok (v', _) -> 
            Assert.AreEqual(v, v')
        | Error msg -> 
            Assert.AreEqual($"Expecting Value {v}", $"Got Error msg: {msg}")

    member this.TestInvalidExpression ctx (e, error) =
        match run (e, ctx) with
        | Ok (v, _) ->
            Assert.AreEqual($"Expected error: {error}", $"Got Value: {v}")
        | Error msg -> 
            Assert.AreEqual(error, msg)

    [<TestMethod>]
    member this.TestClassicLiterals () =
        let ctx = ClassicValueContext.ctx

        [
            // false
            u.Bool false, 
                Value.Bool false
            // 5
            u.Int 5, 
                Value.Int 5
            // (false, 0, 1)
            u.Tuple [u.Bool false; u.Int 0; u.Int 1],
                Value.Tuple [Bool false; Int 0; Int 1]

            // []
            u.Set [],
                Value.Set (Set.ofList [])

            // [false]
            u.Set [u.Bool false],
                Value.Set (Set.ofList [Bool false])

            // [0, 1, 2]
            u.Set [u.Int 0; u.Int 1; u.Int 2],
                Value.Set (Set.ofList [Int 0; Int 1; Int 2])

            // [(false, 0, 0), (true, 0, 1), (true, 1, 1)]
            u.Set [
                u.Tuple [u.Bool false; u.Int 0; u.Int 0]
                u.Tuple [u.Bool true; u.Int 0; u.Int 1]
                u.Tuple [u.Bool true; u.Int 1; u.Int 1]
            ],
                Value.Set (Set.ofList [
                    Tuple [Bool false; Int 0; Int 0]
                    Tuple [Bool true; Int 0; Int 1]
                    Tuple [Bool true; Int 1; Int 1]])
        ]
        |> List.iter (this.TestExpression ctx)


        [
            // Type check:
            u.Set [ u.Tuple [ u.Int 0; u.Int 0]; u.Int 1], "All elements in a set must be of the same type."
        ]
        |> List.iter (this.TestInvalidExpression ctx)
