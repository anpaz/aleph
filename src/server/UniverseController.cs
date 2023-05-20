namespace aleph.server;

using Microsoft.AspNetCore.Mvc;
using Microsoft.FSharp.Collections;
using Microsoft.Quantum.Simulation.Simulators;

using static aleph.kets;

[ApiController]
[Route("[controller]")]
public class UniverseController : ControllerBase
{
    private readonly IGraphsService _graphs;
    private readonly ILogger<GraphController> _logger;

    public UniverseController(ILogger<GraphController> logger, IGraphsService graphs)
    {
        _logger = logger;
        _graphs = graphs;
    }

    [HttpGet("{graphId}")]
    [Produces("text/csv")]
    public async Task<IActionResult> Prepare(string graphId, string ids, int filter = -1)
    {

        if (_graphs.TryFind(graphId, out var graph))
        {
            var ketIds = ids.Split(',').Select(id => int.Parse(id));
            var ctx = new aleph.kets.PrepareContext(qpu: new aleph.qpu.classic.Processor());
            var kets = ListModule.OfSeq(ketIds.Select(k => graph[k]));
            var tempId = 0;

            if (filter > 0)
            {
                tempId = graph.Add(Expression.NewWhere(graph[filter], Operator.Id, ListModule.Empty<KetValue>()));
                kets = ListModule.OfSeq(kets.Append(graph[tempId]));
            }

            var result = prepare(ctx, kets);

            if (result.IsError)
            {
                return new StatusCodeResult(500);
            }
            else
            {
                var universe = result.ResultValue as aleph.qpu.classic.Universe;
                if (universe is null)
                {
                    return new StatusCodeResult(500);
                }

                var writer = Response.BodyWriter;
                var encoding = new System.Text.UTF8Encoding(false);

                var headers = universe.Allocations
                    .OrderBy(kv => kv.Value)
                    .Select(kv => kv.Key)
                    .Where(k => !IsMapId(graph[k]))
                    .ToArray();

                await writer.WriteAsync(encoding.GetBytes(string.Join(',', headers) + "\n"));
                await writer.WriteAsync(encoding.GetBytes(string.Concat(Enumerable.Repeat("##", headers.Length)) + "\n"));

                foreach (var row in universe.State.Rows)
                {
                    await writer.WriteAsync(encoding.GetBytes(string.Join(",", row) + "\n"));
                }

                await writer.CompleteAsync();
                return new EmptyResult();
            }
        }

        return NotFound();
    }

    private bool IsMapId(KetValue ket)
    {
        if (ket.Expression.IsMap)
        {
            var map = (Expression.Map)ket.Expression;
            return map.op.IsId;
        }
        else if (ket.Expression.IsWhere)
        {
            var map = (Expression.Where)ket.Expression;
            return map.clause.IsId;
        }

        return false;
    }
}
