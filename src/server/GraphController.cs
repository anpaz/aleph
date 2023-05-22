using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Microsoft.FSharp.Collections;
using Microsoft.Quantum.Simulation.Simulators;
using static aleph.kets;
namespace aleph.server;

[ApiController]
[Route("[controller]")]
public class GraphController : ControllerBase
{
    private readonly IGraphsService _graphs;
    private readonly ILogger<GraphController> _logger;

    public GraphController(ILogger<GraphController> logger, IGraphsService graphs)
    {
        _logger = logger;
        _graphs = graphs;
    }

    [HttpGet()]
    public IEnumerable<QuantumGraph> GetMany()
    {
        return Enumerable.Empty<QuantumGraph>();
    }

    [HttpGet("~create")]
    [HttpPost("")]
    public string Create()
    {
        var id = Guid.NewGuid().ToString("D");
        var g = new QuantumGraph();
        _graphs.Add(id, g);
        return id;
    }

    [Route("{graphId}")]
    public ActionResult<QuantumGraph> GetGraph(string graphId)
    {
        if (_graphs.TryFind(graphId, out var g))
        {
            return g;
        }

        return NotFound();
    }

    [Route("{graphId}/{ketid}")]
    public ActionResult<GraphNode> GetNode(string graphId, int ketId)
    {
        if (_graphs.TryFind(graphId, out var graph))
        {
            return new GraphNode(ketId, graph);
        }

        return NotFound();
    }

    [Route("{graphId}/literal")]
    public ActionResult<int> Literal(string graphId, [BindRequired] int width) =>
        AddExpression(graphId, Expression.NewLiteral(width));

    [Route("{graphId}/bool")]
    public ActionResult<int> ConstantBool(string graphId, [BindRequired] bool value) =>
        AddExpression(graphId, Expression.NewConstant(value ? 1 : 0));

    [Route("{graphId}/int")]
    public ActionResult<int> ConstantInt(string graphId, [BindRequired] int value) =>
        AddExpression(graphId, Expression.NewConstant(value));

    [Route("{graphId}/where")]
    public ActionResult<int> Where(string graphId, [BindRequired] int id, [BindRequired] string op, [BindRequired] int arg) =>
        AddWhereExpression(graphId, id, OperatorExtensions.Parse(op), arg);

    /// ----------------
    /// Map expressions
    /// ----------------
    [Route("{graphId}/map/not")]
    public ActionResult<int> Not(string graphId) =>
        AddMapExpression(graphId, Operator.Not, new int[] { });

    [Route("{graphId}/map/lte")]
    public ActionResult<int> LetLessThanEquals(string graphId, [BindRequired] int left, [BindRequired] int right) =>
        AddMapExpression(graphId, Operator.LessThanEquals, new int[] { left, right });

    [Route("{graphId}/map/eq")]
    public ActionResult<int> Eq(string graphId, [BindRequired] int left, [BindRequired] int right) =>
        AddMapExpression(graphId, Operator.Eq, new int[] { left, right });

    [Route("{graphId}/map/add")]
    public ActionResult<int> Add(string graphId, [BindRequired] int left, [BindRequired] int right, [BindRequired] int width) =>
        AddMapExpression(graphId, Operator.NewAdd(width), new int[] { left, right });

    [Route("{graphId}/map/multiply")]
    public ActionResult<int> Multiply(string graphId, [BindRequired] int left, [BindRequired] int right, [BindRequired] int width) =>
        AddMapExpression(graphId, Operator.NewMultiply(width), new int[] { left, right });

    [Route("{graphId}/map/if")]
    public ActionResult<int> If(string graphId, [BindRequired] int cond, [BindRequired] int t, [BindRequired] int e) =>
        AddMapExpression(graphId, Operator.If, new int[] { cond, t, e });

    [Route("{graphId}/map/id")]
    public ActionResult<int> Id(string graphId, [BindRequired] int id) =>
        AddMapExpression(graphId, Operator.Id, new int[] { id });

    [Route("{graphId}/map/gt")]
    public ActionResult<int> GreaterThan(string graphId, [BindRequired] int left, [BindRequired] int right) =>
        AddMapExpression(graphId, Operator.GreaterThan, new int[] { left, right });

    [Route("{graphId}/map/in")]
    public ActionResult<int> In(string graphId, int id, [BindRequired] string[] values) =>
        AddMapExpression(graphId, Operator.NewIn(ListModule.OfSeq(values.Select(id => int.Parse(id.Trim())))), new int[] { id });

    [Route("{graphId}/map/and")]
    public ActionResult<int> And(string graphId, [BindRequired] int left, [BindRequired] int right) =>
        AddMapExpression(graphId, Operator.And, new int[] { left, right });

    [Route("{graphId}/map/or")]
    public ActionResult<int> Or(string graphId, [BindRequired] int left, [BindRequired] int right) =>
        AddMapExpression(graphId, Operator.Or, new int[] { left, right });

    private ActionResult<int> AddExpression(string graphId, Expression expression)
    {
        if (_graphs.TryFind(graphId, out var graph))
        {
            return graph.Add(expression);
        }

        return NotFound();
    }

    private ActionResult<int> AddMapExpression(string graphId, [BindRequired] Operator op, int[] argIds)
    {
        if (_graphs.TryFind(graphId, out var graph))
        {
            var kets = ListModule.OfSeq(argIds.Select((k, idx) => graph[k]));
            var expression = Expression.NewMap(op, kets);
            return graph.Add(expression);
        }

        return NotFound();
    }

    private ActionResult<int> AddWhereExpression(string graphId, int id, Operator op, int arg)
    {
        if (_graphs.TryFind(graphId, out var graph))
        {
            var target = graph[id];
            var args = ListModule.OfSeq(new KetValue[] { graph[arg] });
            var expression = Expression.NewWhere(target, op, args);
            return graph.Add(expression);
        }

        return NotFound();
    }

    [Route("{graphId}/~sample")]
    public IActionResult Sample(string graphId, [BindRequired] string ids, int filterId, string backend = "classic")
    {
        if (_graphs.TryFind(graphId, out var graph))
        {
            var ctx = new aleph.kets.PrepareContext(qpu: GetBackend(backend));
            var kets = GetKets(graph, ids);

            var result = (filterId > 0)
                ? sample_when(ctx, kets, graph[filterId])
                : sample(ctx, kets);

            if (result.IsError)
            {
                return new StatusCodeResult(500);
            }
            else
            {
                return Ok(result.ResultValue.ToArray());
            }
        }

        return NotFound();
    }


    [HttpGet("{graphId}/~prepare")]
    public IActionResult Prepare(string graphId, [BindRequired] string ids, int filter = -1, string backend = "classic")
    {
        if (_graphs.TryFind(graphId, out var graph))
        {
            var ctx = new aleph.kets.PrepareContext(qpu: GetBackend(backend));
            var kets = GetKets(graph, ids);

            var result = (filter > 0)
                ? prepare_when(ctx, kets, graph[filter])
                : prepare(ctx, kets);

            if (result.IsError)
            {
                return new StatusCodeResult(500);
            }
            else
            {
                return Ok(result.ResultValue);
            }
        }

        return NotFound();
    }

    [Route("{graphId}/~csv")]
    [Produces("text/plain")]
    public async Task<IActionResult> Csv(string graphId, [BindRequired] string ids, int filter = -1, string backend = "classic")
    {
        if (_graphs.TryFind(graphId, out var graph))
        {
            var ctx = new aleph.kets.PrepareContext(qpu: GetBackend(backend));
            var kets = GetKets(graph, ids);

            var result = (filter > 0)
                ? prepare_when(ctx, kets, graph[filter])
                : prepare(ctx, kets);

            if (result.IsError)
            {
                return new StatusCodeResult(500);
            }
            else
            {
                return await ToCSV(graph, result.ResultValue);
            }
        }

        return NotFound();
    }

    private FSharpList<KetValue> GetKets(QuantumGraph graph, string ids) =>
        ListModule.OfSeq(
                    ids
                    .Split(',')
                    .Select(id => int.Parse(id.Trim()))
                    .Select(k => graph[k]));

    private QPU GetBackend(string backend)
    {
        if (backend == "classic")
        {
            return new aleph.qpu.classic.Processor();
        }
        else if (backend == "sparse" || backend == "qsharp")
        {
            return new aleph.qpu.qsharp.Processor(new SparseSimulator());
        }
        else
        {
            throw new ArgumentException("Unknown backend: " + backend);
        }
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

    private async Task<IActionResult> ToCSV(QuantumGraph graph, IUniverse u)
    {
        var universe = (aleph.qpu.classic.Universe)u;
        System.IO.Pipelines.PipeWriter writer = Response.BodyWriter;
        var encoding = new System.Text.UTF8Encoding(false);

        var headers = universe.Allocations
            .OrderBy(kv => kv.Value)
            .Select(kv => kv.Key)
            .Where(k => !IsMapId(graph[k]))
            .ToArray();

        await writer.WriteAsync(encoding.GetBytes(string.Join(',', headers) + "\n"));

        foreach (var row in universe.State.Rows)
        {
            await writer.WriteAsync(encoding.GetBytes(string.Join(",", row) + "\n"));
        }

        await writer.CompleteAsync();

        return new EmptyResult();
    }
}
