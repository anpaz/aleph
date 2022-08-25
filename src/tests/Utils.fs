namespace aleph.tests

open System.Collections
open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.parser.ast
open aleph.runtime.Eval

// alias for untyped expressions
type u = Expression

module Utils =

    let add_to_context id t e ctx =
        match run (e, ctx) with
        | Ok (v, ctx) ->
            { ctx with heap = ctx.heap.Add (id, v); typeCtx = ctx.typeCtx.Add(id, t)  }
        | Error msg ->
            failwith msg

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
            match answers |> List.tryFind equalAnswer with
            | Some _ -> true
            | None -> false

    let verify_expression (ctx: EvalContext) (e: Expression, answers: Value list) =
        let checkRepeatMeasurement(u, v) =
            for i in 1 .. 5 do
                let v' = measure(u, ctx)
                Assert.AreEqual (v, v')
        // samples the universe exactly once, doesn't check the answer
        let one() =
            let u = prepare(e, ctx)
            let v = measure(u, ctx)
            checkRepeatMeasurement(u, v)
            v
        // tries to get a valid sample by verifying the measurement.
        // if the measurement is invalid, tries one more time since
        // quantum programs can return random values.
        let sample() =
            let v = one()
            if is_valid_answer answers v then
                v
            else
                let v = one()
                Assert.IsTrue(is_valid_answer answers v)
                v

        let mutable i = 0
        let mutable v1 = sample()
        let mutable v2 = sample()
        while (answers.Length > 1 && v2 = v1) do
            v2 <- sample()
            Assert.IsTrue(i < 100)
            i <- i + 1