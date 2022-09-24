namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Microsoft.Quantum.Simulation.Simulators
open Microsoft.Quantum.Simulation.Core

open aleph.parser.ast

open aleph.qsharp
open aleph.runtime.Eval
open aleph.runtime.qpu.qsharp
open aleph.runtime.qpu.qsharp.Convert

open aleph.tests.Utils


[<TestClass>]
type TestQPUQsharp() =
    member this.Context =
        { ClassicValueContext.ctx with qpu = aleph.runtime.qpu.qsharp.Processor(new QuantumSimulator()) }

    [<TestMethod>]
    member this.TestRawLiteral() =
        let sim = new QuantumSimulator()

        let test_one (values: Value list, qubits: int) =
            let bigbang = BigBang.Run(sim).Result
            let v = Set(new Set<Value>(values)) |> toQSet
            let struct (u, o) = ket.Literal.Run(sim, v, bigbang).Result
            printfn "Universe = %A" u
            Assert.AreEqual(int64 (values.Length), u.rows)
            Assert.AreEqual(int64 (qubits + 1), u.columns)

        [ [ Bool true ], BOOL_REGISTER_SIZE
          [ Bool true; Bool false ], BOOL_REGISTER_SIZE
          [ Int 0; Int 1; Int 2 ], INT_REGISTER_DEFAULT_SIZE
          [ Tuple [ Int 0; Int 0 ]; Tuple [ Int 0; Int 1 ]; Tuple [ Int 0; Int 2 ] ],
          INT_REGISTER_DEFAULT_SIZE + INT_REGISTER_DEFAULT_SIZE
          [ Tuple [ Int 0; Bool false; Int 0 ]
            Tuple [ Int 0; Bool true; Int 1 ]
            Tuple [ Int 0; Bool true; Int 2 ]
            Tuple [ Int 2; Bool true; Int 3 ] ],
          INT_REGISTER_DEFAULT_SIZE + BOOL_REGISTER_SIZE + INT_REGISTER_DEFAULT_SIZE ]
        |> List.iter test_one

    [<TestMethod>]
    member this.TestLiteral() =
        let ctx =
            this.Context
            |> add_to_context
                "k"
                (Type.Ket [ Type.Int; Type.Bool ])
                (e.Ket(
                    e.Set
                        [ e.Tuple [ e.Int 0; e.Bool true ]
                          e.Tuple [ e.Int 0; e.Bool false ]
                          e.Tuple [ e.Int 1; e.Bool true ] ]
                ))

        [
          // |>
          e.Ket(e.Set []), []
          //| false >
          e.Ket(e.Bool false), [ Bool false ]
          // | 1; 2; 3 >
          e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ]), [ Int 1; Int 2; Int 3 ]
          // | (false, false), (false, true) >
          e.Ket(
              e.Set
                  [ e.Tuple [ e.Bool false; e.Bool false ]
                    e.Tuple [ e.Bool false; e.Bool true ] ]
          ),
          [ Tuple [ Bool false; Bool false ]; Tuple [ Bool false; Bool true ] ]
          // | (0, false, 0), (0, true, 1), (0, true, 2), (0, true, 3) >
          e.Ket(
              e.Set
                  [ e.Tuple [ e.Int 0; e.Bool false; e.Int 0 ]
                    e.Tuple [ e.Int 0; e.Bool true; e.Int 1 ]
                    e.Tuple [ e.Int 0; e.Bool true; e.Int 2 ]
                    e.Tuple [ e.Int 2; e.Bool true; e.Int 3 ] ]
          ),
          [ Tuple [ Int 0; Bool false; Int 0 ]
            Tuple [ Int 0; Bool true; Int 1 ]
            Tuple [ Int 0; Bool true; Int 2 ]
            Tuple [ Int 2; Bool true; Int 3 ] ]
          // |@,4>
          e.KetAll(e.Int 4), seq { 0..15 } |> Seq.toList |> List.map Int
          // k
          e.Var "k",
          [ Tuple [ Int 0; Bool true ]
            Tuple [ Int 0; Bool false ]
            Tuple [ Int 1; Bool true ] ]
          // k.0
          e.Project(e.Var "k", e.Int 0), [ Int 0; Int 1 ]
          // k.1
          e.Project(e.Var "k", e.Int 1), [ Bool true; Bool false ] ]
        |> List.iter (verify_expression ctx)


    [<TestMethod>]
    member this.TestJoinLiterals() =
        let ctx =
            this.Context
            |> add_to_context
                "k"
                (Type.Ket [ Type.Int; Type.Bool ])
                (e.Ket(
                    e.Set
                        [ e.Tuple [ e.Int 0; e.Bool true ]
                          e.Tuple [ e.Int 0; e.Bool false ]
                          e.Tuple [ e.Int 1; e.Bool true ] ]
                ))
            |> add_to_context "all_1" (Type.Ket [ Type.Int ]) (e.KetAll(e.Int 2))
            |> add_to_context "all_2" (Type.Ket [ Type.Int ]) (e.KetAll(e.Int 2))

        [ e.Join(e.Ket(e.Set []), e.Ket(e.Set [])), []
          // (| false >, | true> )
          e.Join(e.Ket(e.Set [ e.Bool false ]), e.Ket(e.Set [ e.Bool true ])), [ Tuple [ Bool false; Bool true ] ]
          // Join (| 1; 2 >, | true >)
          e.Join(e.Ket(e.Set [ e.Int 1; e.Int 2 ]), e.Ket(e.Set [ e.Bool true ])),
          [ Tuple [ Int 1; Bool true ]; Tuple [ Int 2; Bool true ] ]
          // Join( | (false, false), (false, true) >, |1, 3> )
          e.Join(
              e.Ket(
                  e.Set
                      [ e.Tuple [ e.Bool false; e.Bool false ]
                        e.Tuple [ e.Bool false; e.Bool true ] ]
              ),
              e.Ket(e.Set [ e.Int 1; e.Int 3 ])
          ),
          [ Tuple [ Bool false; Bool false; Int 1 ]
            Tuple [ Bool false; Bool true; Int 1 ]
            Tuple [ Bool false; Bool false; Int 3 ]
            Tuple [ Bool false; Bool true; Int 3 ] ]
          // ( |@,3>, |1,3> )
          e.Join(e.KetAll(e.Int 3), e.Ket(e.Set [ e.Int 1; e.Int 3 ])),
          [ Tuple [ Int 0; Int 1 ]
            Tuple [ Int 0; Int 3 ]
            Tuple [ Int 1; Int 1 ]
            Tuple [ Int 1; Int 3 ]
            Tuple [ Int 2; Int 1 ]
            Tuple [ Int 2; Int 3 ]
            Tuple [ Int 3; Int 1 ]
            Tuple [ Int 3; Int 3 ]
            Tuple [ Int 4; Int 1 ]
            Tuple [ Int 4; Int 3 ]
            Tuple [ Int 5; Int 1 ]
            Tuple [ Int 5; Int 3 ]
            Tuple [ Int 6; Int 1 ]
            Tuple [ Int 6; Int 3 ]
            Tuple [ Int 7; Int 1 ]
            Tuple [ Int 7; Int 3 ] ]
          e.Join(e.Var "k", e.Var "k"),
          [ Tuple [ Int 0; Bool true; Int 0; Bool true ]
            Tuple [ Int 0; Bool false; Int 0; Bool false ]
            Tuple [ Int 1; Bool true; Int 1; Bool true ] ]
          // ( |@,4>, |1, 3> ).[3 - 3]
          e.Project(e.Join(e.KetAll(e.Int 3), e.Ket(e.Set [ e.Int 1; e.Int 3 ])), e.Add(e.Int 3, e.Int -3)),
          seq { 0..15 } |> Seq.toList |> List.map Int
          // ( |@,4>, |1, 3> ).[0 + 1]
          e.Project(e.Join(e.KetAll(e.Int 3), e.Ket(e.Set [ e.Int 1; e.Int 3 ])), e.Add(e.Int 0, e.Int 1)),
          [ Int 1; Int 3 ]
          // let all_1, all_2 = |@,2>;
          // (all_1, all_2, all_1 == all_2)
          Join(Join(Var "all_1", Var "all_2"), Equals(Var "all_1", Var "all_2")),
          seq {
              for i in 0..15 do
                  for j in 0..15 -> Tuple [ Int i; Int j; Bool(i = j) ]
          }
          |> Seq.toList ]
        |> List.iter (verify_expression ctx)


    [<TestMethod>]
    member this.TestBoolExpressions() =
        let ctx =
            this.Context
            |> add_to_context "k1" (Type.Ket [ Type.Bool ]) (e.Ket(e.Set [ e.Bool true; e.Bool false ]))
            |> add_to_context "k2" (Type.Ket [ Type.Bool ]) (e.Ket(e.Set [ e.Bool true; e.Bool false ]))
            |> add_to_context
                "k3"
                (Type.Ket [ Type.Int; Type.Bool; Type.Bool ])
                (e.Ket(
                    e.Set
                        [ e.Tuple [ e.Int 0; e.Bool true; e.Bool true ]
                          e.Tuple [ e.Int 1; e.Bool false; e.Bool true ]
                          e.Tuple [ e.Int 2; e.Bool true; e.Bool false ] ]
                ))

        [
          // ((k1, k2), (k1 && k2))
          e.Join(e.Join(e.Var "k1", e.Var "k2"), e.And(e.Var "k1", e.Var "k2")),
          [ Tuple [ Bool false; Bool false; Bool false ]
            Tuple [ Bool false; Bool true; Bool false ]
            Tuple [ Bool true; Bool false; Bool false ]
            Tuple [ Bool true; Bool true; Bool true ] ]
          // ((k1, k2), (k1 || k2))
          e.Join(e.Join(e.Var "k1", e.Var "k2"), e.Or(e.Var "k1", e.Var "k2")),
          [ Tuple [ Bool false; Bool false; Bool false ]
            Tuple [ Bool false; Bool true; Bool true ]
            Tuple [ Bool true; Bool false; Bool true ]
            Tuple [ Bool true; Bool true; Bool true ] ]
          // (k3.0, k3.1 && k3.2)
          e.Join(e.Project(e.Var "k3", e.Int 0), e.And(e.Project(e.Var "k3", e.Int 1), e.Project(e.Var "k3", e.Int 2))),
          [ Tuple [ Int 0; Bool true ]
            Tuple [ Int 1; Bool false ]
            Tuple [ Int 2; Bool false ] ]

          // (k3.0, !(k3.1 && k3.2))
          e.Join(
              e.Project(e.Var "k3", e.Int 0),
              e.Not(e.And(e.Project(e.Var "k3", e.Int 1), e.Project(e.Var "k3", e.Int 2)))
          ),
          [ Tuple [ Int 0; Bool false ]
            Tuple [ Int 1; Bool true ]
            Tuple [ Int 2; Bool true ] ] ]
        |> List.iter (verify_expression ctx)



    [<TestMethod>]
    member this.TestArithmeticExpressions() =
        let ctx =
            this.Context
            |> add_to_context "k1" (Type.Ket [ Type.Int ]) (e.Ket(e.Set [ e.Int 0; e.Int 1 ]))
            |> add_to_context "k2" (Type.Ket [ Type.Int ]) (e.Ket(e.Set [ e.Int 1; e.Int 2 ]))

        [
          // (k1, k2, k1 + k2)
          e.Join(e.Join(e.Var "k1", e.Var "k2"), e.Add(e.Var "k1", e.Var "k2")),
          [ Tuple [ Int 0; Int 1; Int 1 ]
            Tuple [ Int 0; Int 2; Int 2 ]
            Tuple [ Int 1; Int 1; Int 2 ]
            Tuple [ Int 1; Int 2; Int 3 ] ]
          // let k1 = | (2,2), (2,2), (2,3), (3,3) >
          // (k1, k1.0 + k1.1)
          e.Block(
              [ s.Let(
                    "k1",
                    e.Ket(
                        e.Set
                            [ e.Tuple [ e.Int 2; e.Int 2 ]
                              e.Tuple [ e.Int 2; e.Int 2 ]
                              e.Tuple [ e.Int 2; e.Int 3 ]
                              e.Tuple [ e.Int 3; e.Int 3 ] ]
                    )
                ) ],
              e.Join(e.Var "k1", e.Add(e.Project(e.Var "k1", e.Int 0), e.Project(e.Var "k1", e.Int 1)))
          ),
          [ Tuple [ Int 2; Int 2; Int(4 % (pown 2 INT_REGISTER_DEFAULT_SIZE)) ]
            Tuple [ Int 2; Int 3; Int(5 % (pown 2 INT_REGISTER_DEFAULT_SIZE)) ]
            Tuple [ Int 3; Int 3; Int(6 % (pown 2 INT_REGISTER_DEFAULT_SIZE)) ] ]
          // (k1, k2, k1 * k2)
          e.Join(e.Join(e.Var "k1", e.Var "k2"), e.Multiply(e.Var "k1", e.Var "k2")),
          [ Tuple [ Int 0; Int 1; Int 0 ]
            Tuple [ Int 0; Int 2; Int 0 ]
            Tuple [ Int 1; Int 1; Int 1 ]
            Tuple [ Int 1; Int 2; Int 2 ] ]
          // // (k2, k2 = (k1 * k2))
          // e.Join(
          //     e.Var ("k2"),
          //     e.Add(
          //         e.Var ("k2"),
          //         e.Multiply(e.Var "k1", e.Var "k2"))),
          // [
          //     Tuple [ Int 1; Int 1 ]
          //     Tuple [ Int 2; Int 2 ]
          //     Tuple [ Int 1; Int 2 ]
          //     Tuple [ Int 2; Int 4 ]
          // ]
          ]
        |> List.iter (verify_expression ctx)

    [<TestMethod>]
    member this.TestCallMethod() =
        let ctx = this.Context

        [
          // let colors() = |1,2,3>
          // colors()
          e.Block(
              [ Let(
                    "colors",
                    e.Method(
                        arguments = [],
                        returns = Type.Ket [ Type.Int ],
                        body = e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ])
                    )
                ) ],
              e.CallMethod(e.Var "colors", [])
          ),
          [ Int 1; Int 2; Int 3 ]
          // let colors() = |1,2,3>
          // ( colors(), colors() )
          e.Block(
              [ Let(
                    "colors",
                    e.Method(
                        arguments = [],
                        returns = Type.Ket [ Type.Int ],
                        body = e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ])
                    )
                ) ],
              e.Join(e.CallMethod(e.Var "colors", []), e.CallMethod(e.Var "colors", []))
          ),
          [ Tuple [ Int 1; Int 1 ]
            Tuple [ Int 1; Int 2 ]
            Tuple [ Int 1; Int 3 ]
            Tuple [ Int 2; Int 1 ]
            Tuple [ Int 2; Int 2 ]
            Tuple [ Int 2; Int 3 ]
            Tuple [ Int 3; Int 1 ]
            Tuple [ Int 3; Int 2 ]
            Tuple [ Int 3; Int 3 ] ] ]
        |> List.iter (verify_expression ctx)
