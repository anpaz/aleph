namespace aleph.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.quals.parser.ast
open aleph.quals.runtime.Eval
open aleph.quals.runtime.qpu.classic

open aleph.tests.Utils

[<TestClass>]
(*
    These test take an untyped quantum (ket) expression, and
    prepares the classical processor with the resulting Ket; they
    then verify that the quantum state and the returned columns
    from the preparation matches some expected values.
*)
type TestQPUClassic() =

    member this.QPU = Processor()

    [<TestMethod>]
    member this.TestBasicExpressions() =
        let a = Ket(Literal 2)
        let b = Ket(Literal 2)

        [ [ Ket(Literal 1) ], Universe(state = [ [ 0 ]; [ 1 ] ], outputColumns = [ 0 ], filters = [])
          [ a ], Universe(state = [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ] ], outputColumns = [ 0 ], filters = [])
          [ b ], Universe(state = [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ] ], outputColumns = [ 0 ], filters = [])
          [ a; a ], Universe(state = [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ] ], outputColumns = [ 0; 0 ], filters = [])
          [ a; b ],
          Universe(
              state =
                  [ [ 0; 0 ]
                    [ 0; 1 ]
                    [ 0; 2 ]
                    [ 0; 3 ]
                    [ 1; 0 ]
                    [ 1; 1 ]
                    [ 1; 2 ]
                    [ 1; 3 ]
                    [ 2; 0 ]
                    [ 2; 1 ]
                    [ 2; 2 ]
                    [ 2; 3 ]
                    [ 3; 0 ]
                    [ 3; 1 ]
                    [ 3; 2 ]
                    [ 3; 3 ] ],
              filters = [],
              outputColumns = [ 0; 1 ]
          )
          [ a; Ket(Literal 1) ],
          Universe(
              state = [ [ 0; 0 ]; [ 0; 1 ]; [ 1; 0 ]; [ 1; 1 ]; [ 2; 0 ]; [ 2; 1 ]; [ 3; 0 ]; [ 3; 1 ] ],
              filters = [],
              outputColumns = [ 0; 1 ]
          )
          [ Ket(Literal 1); Ket(Constant 4) ],
          Universe(state = [ [ 0; 4 ]; [ 1; 4 ] ], outputColumns = [ 0; 1 ], filters = []) ]
        |> List.iter (this.TestExpression)

    [<TestMethod>]
    member this.TestAddMultiply() =
        let a = Ket(Literal 1)
        let b = Ket(Literal 1)

        let c = Ket(Literal 2)
                    .Multiply(3)
                    .Where(LessThan, Ket(Constant 3))
                    .Add(b, weight=4)

        let d = Ket(Literal 2)
                    .Multiply(3, weight=4)
                    .Add(b)
                    .Where(LessThan, Ket(Constant 5))

        let e = c.Add(a.Where(Equals, Ket(Constant 0)))

        [ [ Ket(Map(Add(1), [ a; b ])) ],
          Universe(state = [ [ 0; 0; 0 ]; [ 0; 1; 1 ]; [ 1; 0; 1 ]; [ 1; 1; 0 ] ], outputColumns = [ 2 ], filters = [])
          [ a.Add(b) ],
          Universe(state = [ [ 0; 0; 0 ]; [ 0; 1; 1 ]; [ 1; 0; 1 ]; [ 1; 1; 0 ] ], outputColumns = [ 2 ], filters = [])
          [ Ket(Map(Add(2), [ a; b ])) ],
          Universe(state = [ [ 0; 0; 0 ]; [ 0; 1; 1 ]; [ 1; 0; 1 ]; [ 1; 1; 2 ] ], outputColumns = [ 2 ], filters = [])
          [ c ],
          Universe(
              state =
                  [ [ 0; 3; 0; 3; 1; 0; 0 ]
                    [ 0; 3; 0; 3; 1; 1; 1 ]
                    [ 1; 3; 3; 3; 0; 0; 3 ]
                    [ 1; 3; 3; 3; 0; 1; 4 ]
                    [ 2; 3; 2; 3; 1; 0; 2 ]
                    [ 2; 3; 2; 3; 1; 1; 3 ]
                    [ 3; 3; 1; 3; 1; 0; 1 ]
                    [ 3; 3; 1; 3; 1; 1; 2 ] ],
              outputColumns = [ 6 ],
              filters = [ c.Id - 1 ])
          [ d ],
          Universe(
              state =
                  [ [ 0; 3; 0; 0; 0; 5; 1 ]
                    [ 0; 3; 0; 1; 1; 5; 1 ]
                    [ 1; 3; 3; 0; 3; 5; 1 ]
                    [ 1; 3; 3; 1; 4; 5; 1 ]
                    [ 2; 3; 6; 0; 6; 5; 0 ]
                    [ 2; 3; 6; 1; 7; 5; 0 ]
                    [ 3; 3; 9; 0; 9; 5; 0 ]
                    [ 3; 3; 9; 1; 10; 5; 0 ] ],
              outputColumns = [ 4 ],
              filters = [ d.Id ])
              
          [ e ],
          Universe(
              state =
                  [ [ 0; 3; 0; 3; 1; 0; 0; 0; 0; 1; 0 ]
                    [ 0; 3; 0; 3; 1; 0; 0; 1; 0; 0; 1 ]
                    [ 0; 3; 0; 3; 1; 1; 1; 0; 0; 1; 1 ]
                    [ 0; 3; 0; 3; 1; 1; 1; 1; 0; 0; 2 ]
                    [ 1; 3; 3; 3; 0; 0; 3; 0; 0; 1; 3 ]
                    [ 1; 3; 3; 3; 0; 0; 3; 1; 0; 0; 4 ]
                    [ 1; 3; 3; 3; 0; 1; 4; 0; 0; 1; 4 ]
                    [ 1; 3; 3; 3; 0; 1; 4; 1; 0; 0; 5 ]
                    [ 2; 3; 2; 3; 1; 0; 2; 0; 0; 1; 2 ]
                    [ 2; 3; 2; 3; 1; 0; 2; 1; 0; 0; 3 ]
                    [ 2; 3; 2; 3; 1; 1; 3; 0; 0; 1; 3 ]
                    [ 2; 3; 2; 3; 1; 1; 3; 1; 0; 0; 4 ]
                    [ 3; 3; 1; 3; 1; 0; 1; 0; 0; 1; 1 ]
                    [ 3; 3; 1; 3; 1; 0; 1; 1; 0; 0; 2 ]
                    [ 3; 3; 1; 3; 1; 1; 2; 0; 0; 1; 2 ]
                    [ 3; 3; 1; 3; 1; 1; 2; 1; 0; 0; 3 ] ],
              outputColumns = [ 10 ],
              filters = [ c.Id - 1; e.Id - 1 ])
        ]
        |> List.iter (this.TestExpression)


    // [<TestMethod>]
    // member this.TestAddMultiply() =
    //   let a = Ket(2)
    //   let b = Ket(3)

    //     let prelude =
    //         this.Prelude
    //         @ [ s.Let(
    //                 "k1",
    //                 e.Ket(
    //                     e.Set
    //                         [ e.Tuple [ e.Int 0; e.Int 0 ]; e.Tuple [ e.Int 0; e.Int 1 ]; e.Tuple [ e.Int 1; e.Int 1 ] ]
    //                 )
    //             ) ]

    //     [
    //       // k1.0 + k1.1
    //       e.Add(e.Project(e.Var "k1", e.Int 0), e.Project(e.Var "k1", e.Int 1)),
    //       [ [ Int 0; Int 0; Bool true; Int 0 ]; [ Int 0; Int 1; Bool true; Int 1 ]; [ Int 1; Int 1; Bool true; Int 2 ] ],
    //       [ 3 ]
    //       // k1.0 + 1
    //       e.Add(e.Project(e.Var "k1", e.Int 0), e.Int 1),
    //       [ [ Int 0; Int 0; Bool true; Int 1; Int 1 ]
    //         [ Int 0; Int 1; Bool true; Int 1; Int 1 ]
    //         [ Int 1; Int 1; Bool true; Int 1; Int 2 ] ],
    //       [ 4 ]
    //       // k1.0 * 5
    //       e.Multiply(e.Project(e.Var "k1", e.Int 0), e.Int 5),
    //       [ [ Int 0; Int 0; Bool true; Int 5; Int 0 ]
    //         [ Int 0; Int 1; Bool true; Int 5; Int 0 ]
    //         [ Int 1; Int 1; Bool true; Int 5; Int 5 ] ],
    //       [ 4 ]
    //       // k1.0 + | 1, 2, 3 >
    //       e.Add(e.Project(e.Var "k1", e.Int 1), e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ])),
    //       [ [ Int 0; Int 0; Bool true; Int 1; Bool true; Int 1 ]
    //         [ Int 0; Int 0; Bool true; Int 2; Bool true; Int 2 ]
    //         [ Int 0; Int 0; Bool true; Int 3; Bool true; Int 3 ]
    //         [ Int 0; Int 1; Bool true; Int 1; Bool true; Int 2 ]
    //         [ Int 0; Int 1; Bool true; Int 2; Bool true; Int 3 ]
    //         [ Int 0; Int 1; Bool true; Int 3; Bool true; Int 4 ]
    //         [ Int 1; Int 1; Bool true; Int 1; Bool true; Int 2 ]
    //         [ Int 1; Int 1; Bool true; Int 2; Bool true; Int 3 ]
    //         [ Int 1; Int 1; Bool true; Int 3; Bool true; Int 4 ] ],
    //       [ 5 ]
    //       // Join (k1.0, k1.1 + | 1, 2, 3 >)
    //       e.Join(
    //           e.Project(e.Var "k1", e.Int 0),
    //           e.Add(e.Project(e.Var "k1", e.Int 1), e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ]))
    //       ),
    //       [ [ Int 0; Int 0; Bool true; Int 1; Bool true; Int 1 ]
    //         [ Int 0; Int 0; Bool true; Int 2; Bool true; Int 2 ]
    //         [ Int 0; Int 0; Bool true; Int 3; Bool true; Int 3 ]
    //         [ Int 0; Int 1; Bool true; Int 1; Bool true; Int 2 ]
    //         [ Int 0; Int 1; Bool true; Int 2; Bool true; Int 3 ]
    //         [ Int 0; Int 1; Bool true; Int 3; Bool true; Int 4 ]
    //         [ Int 1; Int 1; Bool true; Int 1; Bool true; Int 2 ]
    //         [ Int 1; Int 1; Bool true; Int 2; Bool true; Int 3 ]
    //         [ Int 1; Int 1; Bool true; Int 3; Bool true; Int 4 ] ],
    //       [ 0; 5 ]
    //       // Join (k1.0, k1.1 * | 1, 2, 3 >)
    //       e.Join(
    //           e.Project(e.Var "k1", e.Int 0),
    //           e.Multiply(e.Project(e.Var "k1", e.Int 1), e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ]))
    //       ),
    //       [ [ Int 0; Int 0; Bool true; Int 1; Bool true; Int 0 ]
    //         [ Int 0; Int 0; Bool true; Int 2; Bool true; Int 0 ]
    //         [ Int 0; Int 0; Bool true; Int 3; Bool true; Int 0 ]
    //         [ Int 0; Int 1; Bool true; Int 1; Bool true; Int 1 ]
    //         [ Int 0; Int 1; Bool true; Int 2; Bool true; Int 2 ]
    //         [ Int 0; Int 1; Bool true; Int 3; Bool true; Int 3 ]
    //         [ Int 1; Int 1; Bool true; Int 1; Bool true; Int 1 ]
    //         [ Int 1; Int 1; Bool true; Int 2; Bool true; Int 2 ]
    //         [ Int 1; Int 1; Bool true; Int 3; Bool true; Int 3 ] ],
    //       [ 0; 5 ] ]
    //     |> List.iter (this.TestExpression prelude)

    // [<TestMethod>]
    // member this.TestCompareOps() =
    //     let prelude =
    //         this.Prelude
    //         @ [ s.Let("k1", e.KetAll(e.Int 2)); s.Let("k2", e.KetAll(e.Int 2)) ]

    //     [
    //       // k1 <= k2
    //       e.LessThanEquals(e.Var "k1", e.Var "k2"),
    //       [ [ Int 0; Int 0; Bool true ]
    //         [ Int 0; Int 1; Bool true ]
    //         [ Int 0; Int 2; Bool true ]
    //         [ Int 0; Int 3; Bool true ]
    //         [ Int 1; Int 0; Bool false ]
    //         [ Int 1; Int 1; Bool true ]
    //         [ Int 1; Int 2; Bool true ]
    //         [ Int 1; Int 3; Bool true ]
    //         [ Int 2; Int 0; Bool false ]
    //         [ Int 2; Int 1; Bool false ]
    //         [ Int 2; Int 2; Bool true ]
    //         [ Int 2; Int 3; Bool true ]
    //         [ Int 3; Int 0; Bool false ]
    //         [ Int 3; Int 1; Bool false ]
    //         [ Int 3; Int 2; Bool false ]
    //         [ Int 3; Int 3; Bool true ] ],
    //       [ 2 ]

    //       // k1 > k2
    //       e.GreaterThan(e.Var "k1", e.Var "k2"),
    //       [ [ Int 0; Int 0; Bool false ]
    //         [ Int 0; Int 1; Bool false ]
    //         [ Int 0; Int 2; Bool false ]
    //         [ Int 0; Int 3; Bool false ]
    //         [ Int 1; Int 0; Bool true ]
    //         [ Int 1; Int 1; Bool false ]
    //         [ Int 1; Int 2; Bool false ]
    //         [ Int 1; Int 3; Bool false ]
    //         [ Int 2; Int 0; Bool true ]
    //         [ Int 2; Int 1; Bool true ]
    //         [ Int 2; Int 2; Bool false ]
    //         [ Int 2; Int 3; Bool false ]
    //         [ Int 3; Int 0; Bool true ]
    //         [ Int 3; Int 1; Bool true ]
    //         [ Int 3; Int 2; Bool true ]
    //         [ Int 3; Int 3; Bool false ] ],
    //       [ 2 ] ]
    //     |> List.iter (this.TestExpression prelude)


    // [<TestMethod>]
    // member this.TestJoin() =
    //     let prelude =
    //         this.Prelude
    //         @ [ s.Let(
    //                 "k1",
    //                 e.Ket(
    //                     e.Set
    //                         [ e.Tuple [ e.Int 0; e.Int 0 ]; e.Tuple [ e.Int 0; e.Int 1 ]; e.Tuple [ e.Int 1; e.Int 1 ] ]
    //                 )
    //             )
    //             s.Let("k2", e.Ket(e.Set [ e.Tuple [ e.Int 1 ]; e.Tuple [ e.Int 3 ] ])) ]

    //     [
    //       // let x = 10
    //       // let y = x
    //       // let x = false
    //       // (x, y)
    //       e.Block(
    //           [ s.Let("x", e.Ket(e.Int 10)); s.Let("y", e.Var "x"); s.Let("x", e.Ket(e.Bool false)) ],
    //           e.Join(e.Var "x", e.Var "y")
    //       ),
    //       [ [ Bool false; Int 10 ] ],
    //       [ 0; 1 ]

    //       // Join (k1, k1)
    //       e.Join(e.Var "k1", e.Var "k1"),
    //       [ [ Int 0; Int 0; Bool true ]; [ Int 0; Int 1; Bool true ]; [ Int 1; Int 1; Bool true ] ],
    //       [ 0; 1; 0; 1 ]

    //       // Join (k1, k2)
    //       e.Join(e.Var "k1", e.Var "k2"),
    //       [ [ Int 0; Int 0; Bool true; Int 1; Bool true ]
    //         [ Int 0; Int 0; Bool true; Int 3; Bool true ]
    //         [ Int 0; Int 1; Bool true; Int 1; Bool true ]
    //         [ Int 0; Int 1; Bool true; Int 3; Bool true ]
    //         [ Int 1; Int 1; Bool true; Int 1; Bool true ]
    //         [ Int 1; Int 1; Bool true; Int 3; Bool true ] ],
    //       [ 0; 1; 3 ]

    //       // (Join k1, |true, false>)
    //       e.Join(e.Var "k1", e.Ket(e.Set [ e.Bool true; e.Bool false ])),
    //       [ [ Int 0; Int 0; Bool true; Bool false; Bool true ]
    //         [ Int 0; Int 0; Bool true; Bool true; Bool true ]
    //         [ Int 0; Int 1; Bool true; Bool false; Bool true ]
    //         [ Int 0; Int 1; Bool true; Bool true; Bool true ]
    //         [ Int 1; Int 1; Bool true; Bool false; Bool true ]
    //         [ Int 1; Int 1; Bool true; Bool true; Bool true ] ],
    //       [ 0; 1; 3 ]

    //       // let e1 = k1.1 + 10
    //       // let e2 = k2 + 10
    //       // (Join k1, e2 == e1 )
    //       e.Block(
    //           [ Let("e1", e.Add(e.Project(e.Var "k1", e.Int 1), e.Int 10)); Let("e2", e.Add(e.Var "k2", e.Int 10)) ],
    //           e.Join(e.Var "k1", e.Equals(e.Var "e2", e.Var "e1"))
    //       ),
    //       [ [ Int 0; Int 0; Bool true; Int 1; Bool true; Int 10; Int 11; Int 10; Int 10; Bool false ]
    //         [ Int 0; Int 0; Bool true; Int 3; Bool true; Int 10; Int 13; Int 10; Int 10; Bool false ]
    //         [ Int 0; Int 1; Bool true; Int 1; Bool true; Int 10; Int 11; Int 10; Int 11; Bool true ]
    //         [ Int 0; Int 1; Bool true; Int 3; Bool true; Int 10; Int 13; Int 10; Int 11; Bool false ]
    //         [ Int 1; Int 1; Bool true; Int 1; Bool true; Int 10; Int 11; Int 10; Int 11; Bool true ]
    //         [ Int 1; Int 1; Bool true; Int 3; Bool true; Int 10; Int 13; Int 10; Int 11; Bool false ] ],
    //       [ 0; 1; 9 ]


    //       // let alpha = |3,5>
    //       // let x =
    //       //    let alpha = alpha * 10
    //       //    let x =
    //       //      let alpha = alpha * 20
    //       //      alpha
    //       //    (alpha, x)
    //       // (alpha, x)
    //       e.Block(
    //           [ s.Let("alpha", e.Ket(e.Set [ e.Int 3; e.Int 5 ]))
    //             s.Let(
    //                 "x",
    //                 e.Block(
    //                     [ s.Let("alpha", e.Multiply(e.Var "alpha", e.Int 10))
    //                       s.Let("x", e.Block([ s.Let("alpha", e.Multiply(e.Var "alpha", e.Int 20)) ], e.Var "alpha")) ],
    //                     e.Join(e.Var "alpha", e.Var "x")
    //                 )
    //             ) ],
    //           e.Join(e.Var "alpha", e.Var "x")
    //       ),
    //       [ [ Int 3; Bool true; Int 10; Int 30; Int 20; Int 600 ]
    //         [ Int 5; Bool true; Int 10; Int 50; Int 20; Int 1000 ] ],
    //       [ 0; 3; 5 ]

    //       ]
    //     |> List.iter (this.TestExpression prelude)


    // [<TestMethod>]
    // member this.TestIndex() =
    //     let prelude =
    //         this.Prelude
    //         @ [ s.Let(
    //                 "k1",
    //                 e.Ket(
    //                     e.Set
    //                         [ e.Tuple [ e.Int 0; e.Int 0 ]; e.Tuple [ e.Int 0; e.Int 1 ]; e.Tuple [ e.Int 1; e.Int 1 ] ]
    //                 )
    //             ) ]

    //     [
    //       // k1.0 + k1.[0 + 1]
    //       e.Add(e.Project(e.Var "k1", e.Int 0), e.Project(e.Var "k1", e.Add(e.Int 0, e.Int 1))),
    //       [ [ Int 0; Int 0; Bool true; Int 0 ]; [ Int 0; Int 1; Bool true; Int 1 ]; [ Int 1; Int 1; Bool true; Int 2 ] ],
    //       [ 3 ]
    //       // k1.0 + k1.[0 + 3]        // Index is modular, so 3 == 1
    //       e.Add(e.Project(e.Var "k1", e.Int 0), e.Project(e.Var "k1", e.Add(e.Int 0, e.Int 3))),
    //       [ [ Int 0; Int 0; Bool true; Int 0 ]; [ Int 0; Int 1; Bool true; Int 1 ]; [ Int 1; Int 1; Bool true; Int 2 ] ],
    //       [ 3 ] ]
    //     |> List.iter (this.TestExpression prelude)

    // [<TestMethod>]
    // member this.TestIfQ() =
    //     let prelude =
    //         this.Prelude
    //         @ [ s.Let(
    //                 "k1",
    //                 e.Ket(
    //                     e.Set
    //                         [ e.Tuple [ e.Int 0; e.Int 0 ]; e.Tuple [ e.Int 0; e.Int 1 ]; e.Tuple [ e.Int 1; e.Int 1 ] ]
    //                 )
    //             )
    //             s.Let("k2", e.Ket(e.Set [ e.Tuple [ e.Int 2 ]; e.Tuple [ e.Int 3 ] ])) ]

    //     [
    //       // if k1.1 == k1.0 then k1.1 else 42
    //       e.If(
    //           e.Equals(e.Project(e.Var "k1", e.Int 1), e.Project(e.Var "k1", e.Int 0)),
    //           e.Project(e.Var "k1", e.Int 1),
    //           e.Int 42
    //       ),
    //       [ [ Int 0; Int 0; Bool true; Bool true; Int 42; Int 0 ]
    //         [ Int 0; Int 1; Bool true; Bool false; Int 42; Int 42 ]
    //         [ Int 1; Int 1; Bool true; Bool true; Int 42; Int 1 ] ],
    //       [ 5 ] ]
    //     |> List.iter (this.TestExpression prelude)

    // [<TestMethod>]
    // member this.TestIfClassic() =
    //     let prelude =
    //         this.Prelude
    //         @ [ s.Let(
    //                 "k1",
    //                 e.Ket(
    //                     e.Set
    //                         [ e.Tuple [ e.Int 0; e.Int 0 ]; e.Tuple [ e.Int 0; e.Int 1 ]; e.Tuple [ e.Int 1; e.Int 1 ] ]
    //                 )
    //             )
    //             s.Let("k2", e.Ket(e.Set [ e.Tuple [ e.Int 2 ]; e.Tuple [ e.Int 3 ] ])) ]

    //     [
    //       // if true then k1.1 else 42
    //       e.If(e.Bool true, e.Project(e.Var "k1", e.Int 1), e.Int 42),
    //       [ [ Int 0; Int 0; Bool true ]; [ Int 0; Int 1; Bool true ]; [ Int 1; Int 1; Bool true ] ],
    //       [ 1 ]
    //       // if false then k1.1 else 42
    //       e.If(e.Bool false, e.Project(e.Var "k1", e.Int 1), e.Int 42), [ [ Int 42 ] ], [ 0 ] ]
    //     |> List.iter (this.TestExpression prelude)


    // [<TestMethod>]
    // member this.TestFilterEquals() =
    //     let prelude =
    //         this.Prelude
    //         @ [ s.Let(
    //                 "k1",
    //                 e.Ket(
    //                     e.Set
    //                         [ e.Tuple [ e.Int 0; e.Int 0 ]; e.Tuple [ e.Int 0; e.Int 1 ]; e.Tuple [ e.Int 1; e.Int 1 ] ]
    //                 )
    //             )
    //             s.Let("k2", e.Ket(e.Set [ e.Tuple [ e.Int 2 ]; e.Tuple [ e.Int 3 ] ])) ]

    //     [
    //       // k1.1 == 1
    //       e.Equals(e.Project(e.Var "k1", e.Int 1), e.Int 1),
    //       [ [ Int 0; Int 0; Bool true; Int 1; Bool false ]
    //         [ Int 0; Int 1; Bool true; Int 1; Bool true ]
    //         [ Int 1; Int 1; Bool true; Int 1; Bool true ] ],
    //       [ 4 ]
    //       // (Filter k1, k1.1 == 1, 2)
    //       e.Filter(e.Var "k1", e.Equals(e.Project(e.Var "k1", e.Int 1), e.Int 1)),
    //       [ [ Int 0; Int 1; Bool true; Int 1; Bool true ]; [ Int 1; Int 1; Bool true; Int 1; Bool true ] ],
    //       [ 0; 1 ]
    //       // (Filter k1, k1.0 + k1.1 == 1)
    //       e.Filter(e.Var "k1", e.Equals(e.Add(e.Project(e.Var "k1", e.Int 0), e.Project(e.Var "k1", e.Int 1)), e.Int 1)),
    //       [ [ Int 0; Int 1; Bool true; Int 1; Int 1; Bool true ] ],
    //       [ 0; 1 ]
    //       // (Filter k1.0, k1.1 + | 1, 2, 3 > == |2, 4> )
    //       e.Filter(
    //           e.Project(e.Var "k1", e.Int 1),
    //           e.Equals(
    //               e.Add(e.Project(e.Var "k1", e.Int 1), e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ])),
    //               e.Ket(e.Set [ e.Int 2; e.Int 4 ])
    //           )
    //       ),
    //       [ [ Int 0; Int 0; Bool true; Int 2; Bool true; Int 2; Int 2; Bool true; Bool true ]
    //         [ Int 0; Int 1; Bool true; Int 1; Bool true; Int 2; Int 2; Bool true; Bool true ]
    //         [ Int 0; Int 1; Bool true; Int 3; Bool true; Int 4; Int 4; Bool true; Bool true ]
    //         [ Int 1; Int 1; Bool true; Int 1; Bool true; Int 2; Int 2; Bool true; Bool true ]
    //         [ Int 1; Int 1; Bool true; Int 3; Bool true; Int 4; Int 4; Bool true; Bool true ] ],
    //       [ 1 ] ]
    //     |> List.iter (this.TestExpression prelude)


    // [<TestMethod>]
    // member this.TestBoolOps() =
    //     let prelude =
    //         this.Prelude
    //         @ [ s.Let(
    //                 "k",
    //                 e.Ket(
    //                     e.Set
    //                         [ e.Tuple [ e.Int 0; e.Bool true ]
    //                           e.Tuple [ e.Int 0; e.Bool false ]
    //                           e.Tuple [ e.Int 1; e.Bool true ] ]
    //                 )
    //             ) ]

    //     [ e.Var "k",
    //       [
    //         // Looks like because they are set, they are ordered differently from inputs:
    //         // this might be problematic for tests...
    //         [ Int 0; Bool false; Bool true ]
    //         [ Int 0; Bool true; Bool true ]
    //         [ Int 1; Bool true; Bool true ] ],
    //       [ 0; 1 ]

    //       // not k.1
    //       e.Not(e.Project(e.Var "k", e.Int 1)),
    //       [ [ Int 0; Bool false; Bool true; Bool true ]
    //         [ Int 0; Bool true; Bool true; Bool false ]
    //         [ Int 1; Bool true; Bool true; Bool false ] ],
    //       [ 3 ]

    //       // (false or k.1)
    //       e.Or(e.Bool false, e.Project(e.Var "k", e.Int 1)),
    //       [ [ Bool false; Int 0; Bool false; Bool true; Bool false ]
    //         [ Bool false; Int 0; Bool true; Bool true; Bool true ]
    //         [ Bool false; Int 1; Bool true; Bool true; Bool true ] ],
    //       [ 4 ]

    //       // not (k.0 == 0 and k.1)
    //       e.Not(e.And(e.Equals(e.Project(e.Var "k", e.Int 0), e.Int 0), e.Project(e.Var "k", e.Int 1))),
    //       [ [ Int 0; Bool false; Bool true; Int 0; Bool true; Bool false; Bool true ]
    //         [ Int 0; Bool true; Bool true; Int 0; Bool true; Bool true; Bool false ]
    //         [ Int 1; Bool true; Bool true; Int 0; Bool false; Bool false; Bool true ] ],
    //       [ 6 ] ]
    //     |> List.iter (this.TestExpression prelude)


    // [<TestMethod>]
    // member this.TestCallMethod() =
    //     let prelude = this.Prelude

    //     [
    //       // // let colors() = |1,2,3>
    //       // // colors()
    //       e.Block(
    //           [ Let(
    //                 "colors",
    //                 e.Method(
    //                     arguments = [],
    //                     returns = Type.Ket [ Type.Int ],
    //                     body = e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ])
    //                 )
    //             ) ],
    //           e.CallMethod(e.Var "colors", [])
    //       ),
    //       [ [ Int 1; Bool true ]; [ Int 2; Bool true ]; [ Int 3; Bool true ] ],
    //       [ 0 ]
    //       // let colors() = |1,2,3>
    //       // ( colors(), colors() )
    //       e.Block(
    //           [ Let(
    //                 "colors",
    //                 e.Method(
    //                     arguments = [],
    //                     returns = Type.Ket [ Type.Int ],
    //                     body = e.Ket(e.Set [ e.Int 1; e.Int 2; e.Int 3 ])
    //                 )
    //             ) ],
    //           e.Join(e.CallMethod(e.Var "colors", []), e.CallMethod(e.Var "colors", []))
    //       ),
    //       [ [ Int 1; Bool true; Int 1; Bool true ]
    //         [ Int 1; Bool true; Int 2; Bool true ]
    //         [ Int 1; Bool true; Int 3; Bool true ]
    //         [ Int 2; Bool true; Int 1; Bool true ]
    //         [ Int 2; Bool true; Int 2; Bool true ]
    //         [ Int 2; Bool true; Int 3; Bool true ]
    //         [ Int 3; Bool true; Int 1; Bool true ]
    //         [ Int 3; Bool true; Int 2; Bool true ]
    //         [ Int 3; Bool true; Int 3; Bool true ] ],
    //       [ 0; 2 ]
    //       // let k1 = |0,1>
    //       // let add_one(k: Ket<Int>) = k + 1
    //       // add_one(k1)
    //       e.Block(
    //           [ Let("k1", e.Ket(e.Set [ e.Int 0; e.Int 1 ]))
    //             Let(
    //                 "add_one",
    //                 e.Method(
    //                     arguments = [ "k", (Type.Ket [ Type.Int ]) ],
    //                     returns = Type.Ket [ Type.Int ],
    //                     body = e.Add(e.Var "k", e.Int 1)
    //                 )
    //             ) ],
    //           e.CallMethod(e.Var "add_one", [ e.Var "k1" ])
    //       ),
    //       [ [ Int 0; Bool true; Int 1; Int 1 ]; [ Int 1; Bool true; Int 1; Int 2 ] ],
    //       [ 3 ]
    //       // let k = |0,1>
    //       // let add_one(k: Ket<Int>) = k + 1
    //       // let k = add_one(k)
    //       // k
    //       e.Block(
    //           [ Let("k", e.Ket(e.Set [ e.Int 0; e.Int 1 ]))
    //             Let(
    //                 "add_one",
    //                 e.Method(
    //                     arguments = [ "k", (Type.Ket [ Type.Int ]) ],
    //                     returns = Type.Ket [ Type.Int ],
    //                     body = e.Add(e.Var "k", e.Int 1)
    //                 )
    //             )
    //             Let("k", e.CallMethod(e.Var "add_one", [ e.Var "k" ])) ],
    //           e.Var "k"
    //       ),
    //       [ [ Int 0; Bool true; Int 1; Int 1 ]; [ Int 1; Bool true; Int 1; Int 2 ] ],
    //       [ 3 ]
    //       // let k1 = |0,1>
    //       // let add_one(k: Ket<Int>) = k + 1
    //       // let k2 = add_one(k1)
    //       // let k1 = | 2,3 >
    //       // k2
    //       e.Block(
    //           [ Let("k1", e.Ket(e.Set [ e.Int 0; e.Int 1 ]))
    //             Let(
    //                 "add_one",
    //                 e.Method(
    //                     arguments = [ "k", (Type.Ket [ Type.Int ]) ],
    //                     returns = Type.Ket [ Type.Int ],
    //                     body = e.Add(e.Var "k", e.Int 1)
    //                 )
    //             )
    //             Let("k2", e.CallMethod(e.Var "add_one", [ e.Var "k1" ]))
    //             Let("k1", e.Ket(e.Set [ e.Int 2; e.Int 3 ])) ],
    //           e.Var "k2"
    //       ),
    //       [ [ Int 0; Bool true; Int 1; Int 1 ]; [ Int 1; Bool true; Int 1; Int 2 ] ],
    //       [ 3 ]
    //       // let prepare_bell(a: int, b: int) = Prepare( |(a,a), (b,b)> )
    //       // prepare_bell(2, i1)
    //       e.Block(
    //           [ Let(
    //                 "prepare_bell",
    //                 e.Method(
    //                     arguments = [ "a", Type.Int; "b", Type.Int ],
    //                     returns = Type.Universe [ Type.Int; Type.Int ],
    //                     body =
    //                         e.Prepare(
    //                             e.Ket(e.Set [ e.Tuple [ e.Var "a"; e.Var "a" ]; e.Tuple [ e.Var "b"; e.Var "b" ] ])
    //                         )
    //                 )
    //             ) ],
    //           e.CallMethod(e.Var "prepare_bell", [ e.Int 2; e.Var "i1" ])
    //       ),
    //       [ [ Int 1; Int 1; Bool true ]; [ Int 2; Int 2; Bool true ] ],
    //       [ 0; 1 ] ]
    //     |> List.iter (this.TestExpression prelude)

    // [<TestMethod>]
    // member this.TestMeasure() =
    //     let prelude =
    //         this.Prelude
    //         @ [ s.Let(
    //                 "k",
    //                 e.Ket(
    //                     e.Set
    //                         [ e.Tuple [ e.Int 0; e.Bool true ]
    //                           e.Tuple [ e.Int 0; e.Bool false ]
    //                           e.Tuple [ e.Int 1; e.Bool true ] ]
    //                 )
    //             ) ]

    //     [
    //       // Filter (k, k.0 == 2)
    //       e.Filter(e.Var "k", e.Equals(e.Project(e.Var "k", e.Int 0), e.Int 2)), []
    //       // k
    //       e.Var "k",
    //       [
    //         // Looks like because they are set, they are ordered differently from inputs:
    //         // this might be problematic for tests...
    //         Tuple [ Int 0; Bool false ]
    //         Tuple [ Int 0; Bool true ]
    //         Tuple [ Int 1; Bool true ] ]
    //       // k.1
    //       e.Project(e.Var "k", e.Int 1), [ Bool false; Bool true ]

    //       // (false, k.1)
    //       e.Join(e.Ket(e.Bool false), e.Project(e.Var "k", e.Int 1)),
    //       [ Tuple [ Bool false; Bool false ]; Tuple [ Bool false; Bool true ] ]

    //       // not (k.0 == 0 and k.1)
    //       e.Not(e.And(e.Equals(e.Project(e.Var "k", e.Int 0), e.Int 0), e.Project(e.Var "k", e.Int 1))),
    //       [ Bool false; Bool true ] ]
    //     |> List.iter (verify_expression (prelude, this.QPU))

    // [<TestMethod>]
    // member this.TestRecursiveMethod() =
    //     let prelude = this.Prelude

    //     [
    //       // let sum (acc: Ket<Int>, set:Set<Int>) =
    //       //      if Count(set) == 0 then
    //       //          acc
    //       //      else
    //       //          let elem = Element(set)
    //       //          let rest = Remove(elem, set)
    //       //          sum(acc + elem, rest)
    //       // sum( |10, 20, 30>,  1 .. 4)
    //       e.Block(
    //           [ s.Let(
    //                 "sum",
    //                 e.Method(
    //                     arguments = [ ("acc", Type.Ket [ Type.Int ]); ("set", Type.Set Type.Int) ],
    //                     returns = Type.Ket [ Type.Int ],
    //                     body =
    //                         e.If(
    //                             (e.Equals(e.Count(e.Var "set"), e.Int 0),
    //                              e.Var "acc",
    //                              e.Block(
    //                                  [ s.Let("elem", e.Element(e.Var "set"))
    //                                    s.Let("rest", e.Remove(e.Var "elem", e.Var "set")) ],
    //                                  e.CallMethod(e.Var "sum", [ e.Add(e.Var "acc", e.Var "elem"); e.Var "rest" ])
    //                              ))
    //                         )
    //                 )
    //             ) ],
    //           e.CallMethod(e.Var "sum", [ e.Ket(e.Set [ e.Int 10; e.Int 20; e.Int 30 ]); e.Range(e.Int 1, e.Int 4) ])
    //       ),
    //       [ Int 16; Int 26; Int 36 ] ]
    //     |> List.iter (verify_expression (prelude, this.QPU))


    member this.TestExpression(exprs, expected) =
        printfn "expr: %A" exprs

        let ctx = { qpu = this.QPU }

        match aleph.quals.runtime.Eval.prepare ctx exprs with
        | Ok universe ->
            let actual = universe :?> Universe
            printfn "outputs: %A\nfilters: %A\nmemory: %A\n" actual.Outputs actual.Filters actual.State
            Assert.AreEqual(expected.State, actual.State)
            Assert.AreEqual(expected.Outputs, actual.Outputs)
            Assert.AreEqual(expected.Filters, actual.Filters)
        | Error msg ->
            printfn "e: %A" exprs
            Assert.AreEqual($"Expecting valid expression.", $"Got Error msg: {msg}")
