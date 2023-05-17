namespace aleph.quals.tests

open System.Collections
open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.quals.parser.ast
open aleph.quals.runtime.Eval

module Utils =

    let prepare ctx kets =
        match prepare ctx kets with
        | Ok universe -> universe
        | Error msg ->
            printfn "kets: %A" kets
            Assert.AreEqual($"Expecting valid kets.", $"Got Error msg: {msg}")
            failwith msg

    let measure (u, ctx) =
        let qpu = ctx.qpu
        let m = qpu.Measure u

        match m with
        | Ok t -> t
        | Error msg ->
            printfn "u: %A" u
            Assert.AreEqual($"Expecting valid measurement.", $"Got Error msg: {msg}")
            [] // not reachable.

    let is_valid_answer (answers: int list list) (value: int list) =
        if answers.IsEmpty then
            true
        else
            printfn "Looking for %A in %A" value answers
            answers |> List.contains value

    let AssertSample (qpu: QPU) (kets: Ket list, answers: int list list) =
        let ctx = { qpu = qpu }

        let checkRepeatMeasurement (u, v) =
            for i in 1..5 do
                let v' = measure (u, ctx)
                Assert.AreEqual(v, v')
        // samples the universe exactly once, doesn't check the answer
        let one () =
            let u = prepare ctx kets
            let v = measure (u, ctx)
            checkRepeatMeasurement (u, v)
            v
        // tries to get a valid sample by verifying the measurement.
        // if the measurement is invalid, tries one more time since
        // quantum programs can return random values.
        let sample () =
            let v = one ()

            if is_valid_answer answers v then
                v
            else
                let v = one ()
                Assert.IsTrue(is_valid_answer answers v)
                v

        let mutable i = 0
        let mutable v1 = sample ()
        let mutable v2 = sample ()

        while (answers.Length > 1 && v2 = v1) do
            v2 <- sample ()
            Assert.IsTrue(i < 100)
            i <- i + 1
