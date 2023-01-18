namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Microsoft.Quantum.Simulation.Simulators

open aleph.parser.ast
open aleph.qsharp
open aleph.runtime.Eval
open aleph.runtime.qpu.qsharp
open aleph.runtime.qpu.qsharp.Convert

open aleph.tests.Utils


[<TestClass>]
type TestQPUQsharp() =
    member this.QPU = Processor(new SparseSimulator())

    member this.Prelude = ClassicValueContext.Prelude

    [<TestMethod>]
    member this.TestRawLiteral() =
        let sim = new SparseSimulator()

        let test_one (values: Value list, qubits: int) =
            let bigbang = universe.BigBang.Run(sim).Result
            let v = Set(new Set<Value>(values)) |> toQSet
            let struct (u, o) = ket.Tuples.Run(sim, v, bigbang).Result
            printfn "Universe = %A" u
            Assert.AreEqual(int64 qubits, u.width)

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
        let prelude =
            this.Prelude
            @ [ s.Let(
                    "k",
                    e.Ket(
                        e.Set
                            [ e.Tuple [ e.Int 0; e.Bool true ]
                              e.Tuple [ e.Int 0; e.Bool false ]
                              e.Tuple [ e.Int 1; e.Bool true ] ]
                    )
                ) ]

        [
          //| false >
          e.Ket(e.Bool false), [ Bool false ]
          // | 1; 2; 3 >
          e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ]), [ Int 1; Int 2; Int 3 ]
          // | (false, false), (false, true) >
          e.Ket(e.Set [ e.Tuple [ e.Bool false; e.Bool false ]; e.Tuple [ e.Bool false; e.Bool true ] ]),
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
          e.Var "k", [ Tuple [ Int 0; Bool true ]; Tuple [ Int 0; Bool false ]; Tuple [ Int 1; Bool true ] ]
          // k.0
          e.Project(e.Var "k", e.Int 0), [ Int 0; Int 1 ]
          // k.1
          e.Project(e.Var "k", e.Int 1), [ Bool true; Bool false ] ]
        |> List.iter (verify_expression (prelude, this.QPU))

        [
          // |@,0>
          e.KetAll(e.Int 0), "All ket literals must have a size > 0, got 0"
          // |>
          e.Ket(e.Set []), "All ket literals require a non-empty set." ]
        |> List.iter (verify_invalid_expression (prelude, this.QPU))


    [<TestMethod>]
    member this.TestJoinLiterals() =
        let prelude =
            this.Prelude
            @ [ s.Let(
                    "k",
                    e.Ket(
                        e.Set
                            [ e.Tuple [ e.Int 0; e.Bool true ]
                              e.Tuple [ e.Int 0; e.Bool false ]
                              e.Tuple [ e.Int 1; e.Bool true ] ]
                    )
                )
                s.Let("all_1", e.KetAll(e.Int 2))
                s.Let("all_2", e.KetAll(e.Int 2)) ]

        [
          // ( |@,2>, |@,1> )
          e.Join(e.KetAll(e.Int 2), e.KetAll(e.Int 1)),
          [ Tuple [ Int 0; Bool false ]
            Tuple [ Int 0; Bool true ]
            Tuple [ Int 1; Bool false ]
            Tuple [ Int 1; Bool true ]
            Tuple [ Int 2; Bool false ]
            Tuple [ Int 2; Bool true ]
            Tuple [ Int 3; Bool false ]
            Tuple [ Int 3; Bool true ] ]

          // ( |@,2>, |true> )
          e.Join(e.KetAll(e.Int 2), e.Ket(e.Bool true)),
          [ Tuple [ Int 0; Bool true ]
            Tuple [ Int 1; Bool true ]
            Tuple [ Int 2; Bool true ]
            Tuple [ Int 3; Bool true ] ]

          //      // ( |>, |> )
          //       e.Join(e.Ket(e.Set []), e.Ket(e.Set [])), []
          // (| false >, | true> )
          e.Join(e.Ket(e.Set [ e.Bool false ]), e.Ket(e.Set [ e.Bool true ])), [ Tuple [ Bool false; Bool true ] ]
          // Join (| 1; 2 >, | true >)
          e.Join(e.Ket(e.Set [ e.Int 1; e.Int 2 ]), e.Ket(e.Set [ e.Bool true ])),
          [ Tuple [ Int 1; Bool true ]; Tuple [ Int 2; Bool true ] ]
          // Join( | (false, false), (false, true) >, |1, 3> )
          e.Join(
              e.Ket(e.Set [ e.Tuple [ e.Bool false; e.Bool false ]; e.Tuple [ e.Bool false; e.Bool true ] ]),
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
          e.Join(e.Join(e.Var "all_1", e.Var "all_2"), e.Equals(e.Var "all_1", e.Var "all_2")),
          seq {
              for i in 0..15 do
                  for j in 0..15 -> Tuple [ Int i; Int j; Bool(i = j) ]
          }
          |> Seq.toList ]
        |> List.iter (verify_expression (prelude, this.QPU))


    [<TestMethod>]
    member this.TestBoolExpressions() =
        let prelude =
            this.Prelude
            @ [ s.Let("k1", e.Ket(e.Set [ e.Bool true; e.Bool false ]))
                s.Let("k2", e.Ket(e.Set [ e.Bool true; e.Bool false ]))
                s.Let(
                    "k3",
                    e.Ket(
                        e.Set
                            [ e.Tuple [ e.Int 0; e.Bool true; e.Bool true ]
                              e.Tuple [ e.Int 1; e.Bool false; e.Bool true ]
                              e.Tuple [ e.Int 2; e.Bool true; e.Bool false ] ]
                    )
                ) ]

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
          [ Tuple [ Int 0; Bool true ]; Tuple [ Int 1; Bool false ]; Tuple [ Int 2; Bool false ] ]

          // (k3.0, !(k3.1 && k3.2))
          e.Join(
              e.Project(e.Var "k3", e.Int 0),
              e.Not(e.And(e.Project(e.Var "k3", e.Int 1), e.Project(e.Var "k3", e.Int 2)))
          ),
          [ Tuple [ Int 0; Bool false ]; Tuple [ Int 1; Bool true ]; Tuple [ Int 2; Bool true ] ] ]
        |> List.iter (verify_expression (prelude, this.QPU))


    [<TestMethod>]
    member this.TestIfQuantum() =
        let prelude =
            this.Prelude
            @ [ s.Let(
                    "k1",
                    e.Ket(
                        e.Set [ e.Tuple [ e.Bool true; e.Int 3; e.Int 0 ]; e.Tuple [ e.Bool false; e.Int 1; e.Int 2 ] ]
                    )
                ) ]

        [
          // if k1.0 then k1.1 else k1.2
          e.If(e.Project(e.Var "k1", e.Int 0), e.Project(e.Var "k1", e.Int 1), e.Project(e.Var "k1", e.Int 2)),
          [ Int 3; Int 2 ] ]
        |> List.iter (verify_expression (prelude, this.QPU))


    [<TestMethod>]
    member this.TestIfClassic() =
        let prelude =
            this.Prelude
            @ [ s.Let(
                    "k1",
                    e.Ket(
                        e.Set
                            [ e.Tuple [ e.Int 0; e.Int 0 ]; e.Tuple [ e.Int 0; e.Int 1 ]; e.Tuple [ e.Int 1; e.Int 1 ] ]
                    )
                ) ]

        [
          // if true then k1.1 else 3
          e.If(e.Bool true, e.Project(e.Var "k1", e.Int 1), e.Int 3), [ Int 0; Int 1 ]

          // if false then k1.1 else 3
          e.If(e.Bool false, e.Project(e.Var "k1", e.Int 1), e.Int 3), [ Int 3 ] ]
        |> List.iter (verify_expression (prelude, this.QPU))


    [<TestMethod>]
    member this.TestFilter() =
        let prelude =
            this.Prelude
            @ [ s.Let(
                    "k1",
                    e.Ket(
                        e.Set
                            [ e.Tuple [ e.Bool true; e.Int 3; e.Int 0 ]
                              e.Tuple [ e.Bool false; e.Int 0; e.Int 0 ]
                              e.Tuple [ e.Bool true; e.Int 3; e.Int 3 ]
                              e.Tuple [ e.Bool false; e.Int 1; e.Int 1 ]
                              e.Tuple [ e.Bool false; e.Int 1; e.Int 2 ] ]
                    )
                )
                s.Let("k2", e.KetAll(e.Int 3))
                s.Let("k3", e.KetAll(e.Int 3)) ]

        [
          // (Filter k1, k1.0)
          e.Filter(e.Var "k1", e.Project(e.Var "k1", e.Int 0)),
          [ Tuple [ Bool true; Int 3; Int 0 ]; Tuple [ Bool true; Int 3; Int 3 ] ]

          // (Filter (k2, k3), k2 == k3)
          e.Filter(e.Join(e.Var "k2", e.Var "k3"), e.Equals(e.Var "k2", e.Var "k3")),
          [ Tuple [ Int 0; Int 0 ]
            Tuple [ Int 1; Int 1 ]
            Tuple [ Int 2; Int 2 ]
            Tuple [ Int 3; Int 3 ]
            Tuple [ Int 4; Int 4 ]
            Tuple [ Int 5; Int 5 ]
            Tuple [ Int 6; Int 6 ]
            Tuple [ Int 7; Int 7 ] ] ]
        |> List.iter (verify_expression (prelude, this.QPU))


    [<TestMethod>]
    member this.TestCompareOps() =
        let prelude =
            this.Prelude
            @ [ s.Let("k1", e.KetAll(e.Int 2)); s.Let("k2", e.KetAll(e.Int 2)) ]

        [
          // (k1, k2, k1 <= k2)
          e.Join(e.Var "k1", e.Join(e.Var "k2", e.LessThanEquals(e.Var "k1", e.Var "k2"))),
          [ Tuple [ Int 0; Int 0; Bool true ]
            Tuple [ Int 0; Int 1; Bool true ]
            Tuple [ Int 0; Int 2; Bool true ]
            Tuple [ Int 0; Int 3; Bool true ]
            Tuple [ Int 1; Int 0; Bool false ]
            Tuple [ Int 1; Int 1; Bool true ]
            Tuple [ Int 1; Int 2; Bool true ]
            Tuple [ Int 1; Int 3; Bool true ]
            Tuple [ Int 2; Int 0; Bool false ]
            Tuple [ Int 2; Int 1; Bool false ]
            Tuple [ Int 2; Int 2; Bool true ]
            Tuple [ Int 2; Int 3; Bool true ]
            Tuple [ Int 3; Int 0; Bool false ]
            Tuple [ Int 3; Int 1; Bool false ]
            Tuple [ Int 3; Int 2; Bool false ]
            Tuple [ Int 3; Int 3; Bool true ] ]

          // (k1, k2, k1 <= k2)
          e.Join(e.Var "k1", e.Join(e.Var "k2", e.GreaterThan(e.Var "k1", e.Var "k2"))),
          [ Tuple [ Int 0; Int 0; Bool false ]
            Tuple [ Int 0; Int 1; Bool false ]
            Tuple [ Int 0; Int 2; Bool false ]
            Tuple [ Int 0; Int 3; Bool false ]
            Tuple [ Int 1; Int 0; Bool true ]
            Tuple [ Int 1; Int 1; Bool false ]
            Tuple [ Int 1; Int 2; Bool false ]
            Tuple [ Int 1; Int 3; Bool false ]
            Tuple [ Int 2; Int 0; Bool true ]
            Tuple [ Int 2; Int 1; Bool true ]
            Tuple [ Int 2; Int 2; Bool false ]
            Tuple [ Int 2; Int 3; Bool false ]
            Tuple [ Int 3; Int 0; Bool true ]
            Tuple [ Int 3; Int 1; Bool true ]
            Tuple [ Int 3; Int 2; Bool true ]
            Tuple [ Int 3; Int 3; Bool false ] ] ]
        |> List.iter (verify_expression (prelude, this.QPU))

    [<TestMethod>]
    member this.TestArithmeticExpressions() =
        let prelude =
            this.Prelude
            @ [ s.Let("k1", e.Ket(e.Set [ e.Int 0; e.Int 1 ])); s.Let("k2", e.Ket(e.Set [ e.Int 1; e.Int 2 ])) ]

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
          // (k2, k1, k2 == (k1 * k2))
          e.Join(e.Var("k2"), e.Join(e.Var("k1"), e.Equals(e.Var("k2"), e.Multiply(e.Var "k1", e.Var "k2")))),
          [ Tuple [ Int 1; Int 0; Bool false ]
            Tuple [ Int 1; Int 1; Bool true ]
            Tuple [ Int 2; Int 0; Bool false ]
            Tuple [ Int 2; Int 1; Bool true ] ]
          // (k2, k1 + 1, k2 < (k1 + 1))
          e.Join(
              e.Var("k2"),
              e.Join(e.Add(e.Var "k1", e.Int 1), e.LessThanEquals(e.Var("k2"), e.Add(e.Var "k1", e.Int 1)))
          ),
          [ Tuple [ Int 1; Int 1; Bool true ]
            Tuple [ Int 1; Int 2; Bool true ]
            Tuple [ Int 2; Int 1; Bool false ]
            Tuple [ Int 2; Int 2; Bool true ] ] ]
        |> List.iter (verify_expression (prelude, this.QPU))

    [<TestMethod>]
    member this.TestCallMethod() =
        let prelude = this.Prelude

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
        |> List.iter (verify_expression (prelude, this.QPU))
