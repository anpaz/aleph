namespace aleph.server;

using Microsoft.AspNetCore.Mvc;
using Microsoft.FSharp.Collections;
using Microsoft.Quantum.Simulation.Simulators;

using static aleph.runtime.Eval;

[ApiController]
[Route("[controller]")]
public class SampleController : ControllerBase
{
    private readonly IGraphsService _graphs;
    private readonly ILogger<GraphController> _logger;

    public SampleController(ILogger<GraphController> logger, IGraphsService graphs)
    {
        _logger = logger;
        _graphs = graphs;
    }

    [HttpPost("classic/{graphId}/{ketId}")]
    public IActionResult SampleWithClassic(string graphId, int ketId) =>
        Sample(graphId, ketId, new aleph.runtime.qpu.classic.Processor());


    [HttpPost("qsharp/{graphId}/{ketId}")]
    public IActionResult SampleWithQsharp(string graphId, int ketId) =>
        Sample(graphId, ketId, new aleph.runtime.qpu.qsharp.Processor(new SparseSimulator()));

    private IActionResult Sample(string graphId, int ketId, QPU qpu)
    {
        if (_graphs.TryFind(graphId, out var graph)) {
            var ctx = new aleph.runtime.qpu.classic.QuantumContext(
                graph: graph,
                state: ListModule.Empty<FSharpList<Value>>(),
                allocations: new FSharpMap<int, runtime.qpu.classic.ColumnIndex>(Enumerable.Empty<Tuple<int, runtime.qpu.classic.ColumnIndex>>())
            );

            var prepare = qpu.Prepare(ketId, graph);
            if (prepare.IsError) {
                return new StatusCodeResult(500);
            } else {
                var universe = prepare.ResultValue;
                var measure = qpu.Measure(universe);
                if (measure.IsError) {
                    return new StatusCodeResult(500);
                } else {
                    return Ok(measure.ResultValue);
                }
            }
        }

        return NotFound();
    }
}
