namespace aleph.quals.tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open aleph.kets

module Utils =

    let is_valid_answer (answers: int list list) (value: int list) =
        if answers.IsEmpty then
            true
        else
            printfn "Looking for %A in %A" value answers
            answers |> List.contains value

    let AssertSampleWithFilter (qpu: QPU) (kets: KetValue list, filter: KetValue option, answers: int list list) =
        let ctx = { qpu = qpu}

        // calls the qpu directly to prepare and sample a universe.
        // it samples the same universe multiple times, checks each time it gets the same answer
        let raw_sample ctx (kets: KetValue list, filter: KetValue option) =
            let qpu = ctx.qpu
            let prep_kets = 
                match filter with
                | Some filter -> KetValue(Where (filter, Id, [])) :: kets
                | None -> kets
            let u = qpu.Prepare prep_kets |> Result.toList |> Seq.head
            let v = u.Sample(kets) |> Result.toList |> Seq.head
            for i in 1..5 do
                let v' = u.Sample(kets) |> Result.toList |> Seq.head
                Assert.AreEqual(v, v')
            v
        // calls the qpu using the built-in sample methods
        let regular_sample ctx (k, f) =
            match f with
            | None -> sample ctx k
            | Some f -> sample_when ctx (k, f)
            |> Result.toList |> Seq.head
        // calls a sample method, and makes sure it gets a valid answer.
        let rec try_sample max_tries lambda =
            let v = lambda ctx (kets, filter)
            if is_valid_answer answers v then
                v
            else if (max_tries > 1) then
                try_sample (max_tries - 1) lambda
            else
                Assert.Fail("Couldn't get a valid measurement.")
                []

        let max_tries = 2
        let mutable i = 0
        let mutable v1 = try_sample max_tries raw_sample
        let mutable v2 = try_sample max_tries regular_sample
        
        while (answers.Length > 1 && v2 = v1) do
            v2 <- try_sample max_tries regular_sample
            Assert.IsTrue(i < 100)
            i <- i + 1

    let AssertSample (qpu: QPU) (kets: KetValue list, answers: int list list) =
        AssertSampleWithFilter qpu (kets, None, answers)

