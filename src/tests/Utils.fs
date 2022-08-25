namespace aleph.tests

open System.Collections
open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.parser.ast
open aleph.runtime.Eval

// alias for untyped expressions
type u = Expression

module Utils =

    let prepare (e, ctx) = 
        match run(u.Prepare e, ctx) with
        | Ok (Universe universe, _) ->
            universe
        | Ok (v, _) ->
            printfn "e: %A" e
            Assert.AreEqual($"Expecting Universe value.", $"Got {v}")
            { new IUniverse with
                  member this.CompareTo(obj) = failwith "Not Implemented" } // not reachable
        | Error msg ->
            printfn "e: %A" e
            Assert.AreEqual($"Expecting valid expression.", $"Got Error msg: {msg}")
            { new IUniverse with
                  member this.CompareTo(obj) = failwith "Not Implemented" } // not reachable


    let measure(u, ctx) =
        let qpu = ctx.qpu
        let m = qpu.Measure u
        match m with 
        | Ok t ->
            t
        | Error msg ->
            printfn "u: %A" u
            Assert.AreEqual($"Expecting valid measurement.", $"Got Error msg: {msg}")
            Bool false // not reachable.


    let is_valid_answer (answers: Value list) (value: Value) =
        if answers.IsEmpty then 
            true
        else
            printfn "Looking for %A in %A" value answers
            let equalAnswer i =
                StructuralComparisons.StructuralEqualityComparer.Equals(value, i)
            answers |> List.find equalAnswer |> ignore
            true

    let verify_expression (ctx: ValueContext) (e: Expression, answers: Value list) =
        let qpu = ctx.qpu
        let checkRepeatMeasurement(u, v) =
            for i in 1 .. 5 do
                let v' = measure(u, ctx)
                Assert.AreEqual (v, v')

        let u1 = prepare(e, ctx)
        let v1 = measure(u1, ctx)
        checkRepeatMeasurement(u1, v1)
        Assert.IsTrue(is_valid_answer answers v1)

        let mutable i = 0
        let mutable u2 = prepare(e, ctx)
        let mutable v2 = measure(u2, ctx)
        checkRepeatMeasurement(u2, v2)
        Assert.IsTrue(is_valid_answer answers v1)
        while (answers.Length > 0 && v2 = v1) do
            u2 <- prepare(e, ctx)
            v2 <- measure(u2, ctx)
            checkRepeatMeasurement(u2, v2)
            Assert.IsTrue(is_valid_answer answers v1)
            Assert.IsTrue(i < 100)
            i <- i + 1