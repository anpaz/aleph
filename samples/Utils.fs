
module Utils
    open aleph.runtime.Eval
    open Microsoft.Quantum.Simulation.Simulators
    open Microsoft.Quantum.IQSharp.ExecutionPathTracer

    let run qpu program =
        let context = { 
            heap = Map.empty
            typeCtx = Map.empty
            qpu =  qpu
        }

        match run (program, context) with
        | Ok (v, _) ->
            printfn $"\nresult: {v}"
            0
        | Error msg ->
            printfn $"\n!! Failed: {msg} !!"
            1

    let wrapup code =
        printfn ""
        printfn ":ℵ-0.4:"
        code

    let simulate program = program |> run (aleph.runtime.qpu.classic.Processor())

    let trace program = 
        let tracer = new ExecutionPathTracer()
        let sim = (new QuantumSimulator()).WithExecutionPathTracer(tracer)

        let r =
            program 
            |> run (aleph.runtime.qpu.qsharp.Processor(sim))

        System.IO.File.WriteAllText ("circuit.json", tracer.GetExecutionPath().ToJson())
        r
