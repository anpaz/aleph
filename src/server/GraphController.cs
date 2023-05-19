using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Microsoft.FSharp.Collections;

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
    

    [HttpGet("-create")]
    [HttpPost("")]
    public string Create()
    {
        var id = Guid.NewGuid().ToString("D");
        var g = new QuantumGraph();
        _graphs.Add(id, g);
        return id;
    }

    [HttpGet("{id}")]
    public ActionResult<QuantumGraph> GetGraph(string id)
    {
        if (_graphs.TryFind(id, out var g)) {
            return g;
        } 

        return NotFound();
    }
    
    [HttpGet("{id}/{ketid}")]
    public ActionResult<GraphNode> GetNode(string id, int ketId)
    {
        if (_graphs.TryFind(id, out var g)) {
            return new GraphNode(ketId, g);
        }

        return NotFound();
    }
    
    [HttpGet("{id}/literal")]
    [HttpPost("{id}/literal")]
    public ActionResult<int> Literal(string id, [BindRequired] int width) =>
        AddExpression(id, Expression.NewLiteral(width));
    
    [HttpGet("{id}/bool")]
    [HttpPost("{id}/bool")]
    public ActionResult<int> ConstantBool(string id, [BindRequired] bool value) => 
        AddExpression(id, Expression.NewConstant(value? 1 : 0));

    [HttpGet("{id}/int")]
    [HttpPost("{id}/int")]
    public ActionResult<int> ConstantInt(string id, [BindRequired] int value) => 
        AddExpression(id, Expression.NewConstant(value));

    // [HttpGet("{id}/where")]
    // [HttpPost("{id}/where")]
    // public ActionResult<int> Where(string id, [BindRequired] int ket, [BindRequired] int filter) => 
    //     BasicExpression(id, KetExpression.NewFilter(ket, filter));

    /// ----------------
    /// Map expressions
    /// ----------------
    [HttpGet("{id}/add")]
    [HttpPost("{id}/add")]
    public ActionResult<int> Add(string id, [BindRequired] int left, [BindRequired] int right, [BindRequired] int width) => 
        AddMapExpression(id, Operator.NewAdd(width), new int[] {left, right});
    

    [HttpGet("{id}/multiply")]
    [HttpPost("{id}/multiply")]
    public ActionResult<int> Multiply(string id, [BindRequired] int left, [BindRequired] int right, [BindRequired] int width) => 
        AddMapExpression(id, Operator.NewMultiply(width), new int[] {left, right});
    
    [HttpGet("{id}/not")]
    [HttpPost("{id}/not")]
    public ActionResult<int> Not(string id) => 
        AddMapExpression(id, Operator.Not, new int[] {});

    [HttpGet("{id}/id")]
    [HttpPost("{id}/id")]
    public ActionResult<int> Id(string id) => 
        AddMapExpression(id, Operator.Id, new int[] {});

    [HttpGet("{id}/and")]
    [HttpPost("{id}/and")]
    public ActionResult<int> And(string id, [BindRequired] int left, [BindRequired] int right) => 
        AddMapExpression(id, Operator.And, new int[] {left, right});
    
    [HttpGet("{id}/or")]
    [HttpPost("{id}/or")]
    public ActionResult<int> Or(string id, [BindRequired] int left, [BindRequired] int right) => 
        AddMapExpression(id, Operator.Or, new int[] {left, right});
    
    [HttpGet("{id}/eq")]
    [HttpPost("{id}/eq")]
    public ActionResult<int> Eq(string id, [BindRequired] int left, [BindRequired] int right) => 
        AddMapExpression(id, Operator.Eq, new int[] {left, right});

    [HttpGet("{id}/lte")]
    [HttpPost("{id}/lte")]
    public ActionResult<int> LetLessThanEquals(string id, [BindRequired] int left, [BindRequired] int right) => 
        AddMapExpression(id, Operator.LessThanEquals, new int[] {left, right});
    
    [HttpGet("{id}/gt")]
    [HttpPost("{id}/gt")]
    public ActionResult<int> GreaterThan(string id, [BindRequired] int left, [BindRequired] int right) => 
        AddMapExpression(id, Operator.GreaterThan, new int[] {left, right});
    
    [HttpGet("{id}/if")]
    [HttpPost("{id}/if")]
    public ActionResult<int> If(string id, [BindRequired] int cond, [BindRequired] int t, [BindRequired] int e) => 
        AddMapExpression(id, Operator.If, new int[] {cond, t, e});
    

    private ActionResult<int> AddExpression(string graphId, Expression expression) {
        if (_graphs.TryFind(graphId, out var g)) {
            return g.Add(expression);
        }

        return NotFound();
    }

    private ActionResult<int> AddMapExpression(string graphId, Operator op, int[] argIds) {
        if (_graphs.TryFind(graphId, out var g)) {
            var kets = ListModule.OfSeq(argIds.Select((k, idx) => g[k]));
            var expression = Expression.NewMap(op, kets);
            return g.Add(expression);
        }

        return NotFound();
    }

}
