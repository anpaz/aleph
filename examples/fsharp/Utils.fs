module Utils

open aleph.kets


let wrapup code =
    printfn ""
    printfn ":ℵ-0.9:"
    code

let simulate program =
    let result = program()
    printfn "result: %A" result

// let estimate program =
//     let res = new ResourcesEstimator()
//     //let tracer = new ExecutionPathTracer()
//     let sim = res // (res).WithExecutionPathTracer(tracer)

//     printfn ("==> Starting resources estimation...")
//     apply (program, aleph.runtime.qpu.qsharp.Processor(sim)) |> ignore

//     let estimate (key: string) =
//         let row =
//             res.Data.Rows
//             |> Seq.cast<System.Data.DataRow>
//             |> Seq.find (fun r -> r.[0] = key)

//         int64 (row.[1].ToString())

//     let width = estimate "Width"
//     let depth = estimate "Depth"
//     printfn "       resources: width: %d; depth: %d" width depth

//     // if depth < 1000 then
//     //     printf ("==> Saving circuit... ")
//     //     System.IO.File.WriteAllText("circuit.json", tracer.GetExecutionPath().ToJson())
//     //     printfn ("done.\n")

//     (width, depth)

// let qsharp program =
//     printfn ("==> Starting quantum execution...")
//     let sim = new SparseSimulator()
//     program |> run (aleph.runtime.qpu.qsharp.Processor(sim)) |> ignore
//     0
