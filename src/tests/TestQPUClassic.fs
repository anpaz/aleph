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
                    .Add(b, width=4)

        let d = Ket(Literal 2)
                    .Multiply(3, width=4)
                    .Add(b)
                    .Where(LessThan, Ket(Constant 5))

        let e = c.Add(a.Where(IsZero))

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
              filters = [ 4 ])

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
              filters = [ 6 ])
              
          [ e ],
          Universe(
              state =
                  [ [ 0; 3; 0; 3; 1; 0; 0; 0; 1; 0 ]
                    [ 0; 3; 0; 3; 1; 0; 0; 1; 0; 1 ]
                    [ 0; 3; 0; 3; 1; 1; 1; 0; 1; 1 ]
                    [ 0; 3; 0; 3; 1; 1; 1; 1; 0; 2 ]
                    [ 1; 3; 3; 3; 0; 0; 3; 0; 1; 3 ]
                    [ 1; 3; 3; 3; 0; 0; 3; 1; 0; 4 ]
                    [ 1; 3; 3; 3; 0; 1; 4; 0; 1; 4 ]
                    [ 1; 3; 3; 3; 0; 1; 4; 1; 0; 5 ]
                    [ 2; 3; 2; 3; 1; 0; 2; 0; 1; 2 ]
                    [ 2; 3; 2; 3; 1; 0; 2; 1; 0; 3 ]
                    [ 2; 3; 2; 3; 1; 1; 3; 0; 1; 3 ]
                    [ 2; 3; 2; 3; 1; 1; 3; 1; 0; 4 ]
                    [ 3; 3; 1; 3; 1; 0; 1; 0; 1; 1 ]
                    [ 3; 3; 1; 3; 1; 0; 1; 1; 0; 2 ]
                    [ 3; 3; 1; 3; 1; 1; 2; 0; 1; 2 ]
                    [ 3; 3; 1; 3; 1; 1; 2; 1; 0; 3 ] ],
              outputColumns = [ 9 ],
              filters = [ 4; 8 ])
        ]
        |> List.iter (this.TestExpression)


    [<TestMethod>]
    member this.TestBoolean() =
        let a = Ket(Literal 1)
        let b = Ket(Literal 1)
        let c = a.Add(b, width=2).Where(LessThan, 2)

        // TODO: The compiler should know that
        //   d = a.LessThan(b)
        //   e = a.LessThan(b)
        // can point to the same ket.
        let d = a.LessThan(b).And(b.LessThan(1))
        let e = a.LessThan(b).Or(b.LessThan(1))
        let f = e.Not().Where(Equals, 1)

        [ [ a.LessThan(b) ],
          Universe(state = [ [ 0; 0; 0 ]; [ 0; 1; 1 ]; [ 1; 0; 0 ]; [ 1; 1; 0 ] ], outputColumns = [ 2 ], filters = [])

          [ b.LessThan(1) ],
          Universe(state = [ [ 0; 1; 1 ]; [ 1; 1; 0 ] ], outputColumns = [ 2 ], filters = [])

          [ c ],
          Universe(state = [ 
            [ 0; 0; 0; 2; 1 ]
            [ 0; 1; 1; 2; 1 ]
            [ 1; 0; 1; 2; 1 ]
            [ 1; 1; 2; 2; 0 ] ], outputColumns = [ 2 ], filters = [ 4 ])

          [ d; e ],
          Universe(state = [
            [ 0; 0; 0; 1; 1; 0; 0; 1; 1; 1 ]
            [ 0; 1; 1; 1; 0; 0; 1; 1; 0; 1 ]
            [ 1; 0; 0; 1; 1; 0; 0; 1; 1; 1 ]
            [ 1; 1; 0; 1; 0; 0; 0; 1; 0; 0 ] ], outputColumns = [ 5; 9 ], filters = [])

          [ f ],
          Universe(state = [
            [ 0; 0; 0; 1; 1; 1; 0; 1; 0 ]
            [ 0; 1; 1; 1; 0; 1; 0; 1; 0 ]
            [ 1; 0; 0; 1; 1; 1; 0; 1; 0 ]
            [ 1; 1; 0; 1; 0; 0; 1; 1; 1 ] ], outputColumns = [ 6 ], filters = [ 8 ])
        ]
        |> List.iter (this.TestExpression)

    [<TestMethod>]
    member this.TestIn() =
        let a = Ket(Literal 2)

        [ [ a.In [0; 2] ],
            Universe(state = [
                [ 0; 1 ]
                [ 1; 0 ]
                [ 2; 1 ]
                [ 3; 0 ]
            ], outputColumns = [ 1 ], filters = [])
            [ a.Where(In [0; 2]) ],
            Universe(state = [
                [ 0; 1 ]
                [ 1; 0 ]
                [ 2; 1 ]
                [ 3; 0 ]
            ], outputColumns = [ 0 ], filters = [ 1 ])
            
            [ a; a.Where(In []); a.In [1;2;3;4] ],
            Universe(state = [
                [ 0; 0; 0 ]
                [ 1; 0; 1 ]
                [ 2; 0; 1 ]
                [ 3; 0; 1 ]
            ], outputColumns = [ 0; 0; 2 ], filters = [ 1 ])
        ]
        |> List.iter (this.TestExpression)

        
    [<TestMethod>]
    member this.TestIf() =
        let a = Ket(Literal 2)
        let b = a.Add(4, width=3)

        let c = a.In([1;2]).Choose(a, b)

        [ [ c ],
            Universe(state = [
                [ 0; 0; 4; 4; 4 ]
                [ 1; 1; 4; 5; 1 ]
                [ 2; 1; 4; 6; 2 ]
                [ 3; 0; 4; 7; 7 ]
            ], outputColumns = [ 4 ], filters = [])
        ]
        |> List.iter (this.TestExpression)


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
