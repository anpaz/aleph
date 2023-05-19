namespace aleph.server;

using Microsoft.AspNetCore.Mvc;
using Microsoft.FSharp.Collections;
using Microsoft.Quantum.Simulation.Simulators;

using static aleph.kets;

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

    [HttpGet("classic/{graphId}")]
    [HttpPost("classic/{graphId}")]
    public IActionResult SampleWithClassic(string graphId, [FromQuery]int[] id, int filterId = -1) =>
        Sample(graphId,
               id,
               filterId,
               new aleph.qpu.classic.Processor());


    [HttpGet("qsharp/{graphId}")]
    [HttpPost("qsharp/{graphId}")]
    public IActionResult SampleWithQsharp(string graphId, [FromQuery]int[] id, int filterId=-1) =>
        Sample(graphId, id, filterId, new aleph.qpu.qsharp.Processor(new SparseSimulator()));

    private IActionResult Sample(string graphId, IEnumerable<int> ketIds, int filterId, aleph.kets.QPU qpu)
    {
        if (_graphs.TryFind(graphId, out var graph)) {
            var ctx = new aleph.kets.PrepareContext( qpu: qpu);
            var kets = ListModule.OfSeq(ketIds.Select((k, idx) => graph[k]));

            Console.WriteLine($"Sampling: {kets}; with filter {filterId}");
            
            var result = (filterId > 0)
                ? sample_when(ctx, kets, graph[filterId])
                : sample(ctx, kets);

            if (result.IsError) {
                return new StatusCodeResult(500);
            } else {
                return Ok(result.ResultValue.ToArray());
            }
        }

        return NotFound();
    }
}
