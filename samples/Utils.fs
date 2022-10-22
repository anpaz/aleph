module Utils

open aleph.runtime.Eval
open Microsoft.Quantum.Simulation.Simulators
open Microsoft.Quantum.IQSharp.ExecutionPathTracer

let run qpu program =
    match start (program, qpu) with
    | Ok (v, _) ->
        printfn $"\nresult: {v}"
        0
    | Error msg ->
        printfn $"\n!! Failed: {msg} !!"
        1

let wrapup code =
    printfn ""
    printfn ":ℵ-0.5:"
    code

let simulate program =
    program |> run (aleph.runtime.qpu.classic.Processor())

let estimate program =
    let res = new ResourcesEstimator()
    let tracer = new ExecutionPathTracer()
    let sim = (res).WithExecutionPathTracer(tracer)

    printfn ("==> Starting resources estimation...\n")
    program |> run (aleph.runtime.qpu.qsharp.Processor(sim)) |> ignore

    let estimate (key: string) = 
        let row = 
            res.Data.Rows 
            |> Seq.cast<System.Data.DataRow>
            |> Seq.find( fun r -> r.[0] = key)
        int64 (row.[1].ToString())

    let width = estimate "Width"
    let depth = estimate "Depth"
    printfn "\n==> Estimated resources:\n  - Width: %d\n  - Depth: %d" width depth

    printf ("==> Saving circuit... ")
    System.IO.File.WriteAllText("circuit.json", tracer.GetExecutionPath().ToJson())
    printfn("done.")
    
    if width < 35 then
        printfn ("==> Starting quantum execution...")
        let sim = new QuantumSimulator()
        program |> run (aleph.runtime.qpu.qsharp.Processor(sim)) |> ignore
        0
    else 
        1
