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
    let sim = res // (res).WithExecutionPathTracer(tracer)
    let sim = new QuantumSimulator()

    let r = program |> run (aleph.runtime.qpu.qsharp.Processor(res))
    res.Data.Rows 
    |> Seq.cast<System.Data.DataRow> 
    |> Seq.iter (fun r -> 
        r.ItemArray |> Seq.iter (printfn "r: %A"))

    let r = program |> run (aleph.runtime.qpu.qsharp.Processor(sim))

    //System.IO.File.WriteAllText("circuit.json", tracer.GetExecutionPath().ToJson())
    r
