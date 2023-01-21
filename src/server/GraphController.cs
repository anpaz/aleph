using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Microsoft.FSharp.Collections;
using static aleph.runtime.Eval;

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
    public IEnumerable<QuantumGraph> GetAll()
    {
        return Enumerable.Empty<QuantumGraph>();
    }
    

    [HttpPost("")]
    public string Create()
    {
        var id = Guid.NewGuid().ToString("D");
        var g = QuantumGraph.empty;
        _graphs.Add(id, g);
        return id;
    }

    [HttpGet("{id}")]
    public ActionResult<QuantumGraph> GetOne(string id)
    {
        if (_graphs.TryFind(id, out var g)) {
            return g;
        }

        return NotFound();
    }
    

    [HttpPost("{id}/literal")]
    public ActionResult<int> Literal(string id, [BindRequired] int size) =>
        BasicExpression(id, KetExpression.NewLiteral(size));
    
    [HttpGet("{id}/join")]
    [HttpPost("{id}/join")]
    public ActionResult<int> Join(string id, [BindRequired] int left, [BindRequired] int right) => 
        BasicExpression(id, KetExpression.NewJoin(ListModule.OfSeq(new int[] { left, right })));
    
    [HttpGet("{id}/project")]
    [HttpPost("{id}/project")]
    public ActionResult<int> Project(string id, [BindRequired] int ket, [BindRequired] int position) => 
        BasicExpression(id, KetExpression.NewProject(ket, position));

    [HttpGet("{id}/filter")]
    [HttpPost("{id}/filter")]
    public ActionResult<int> Filter(string id, [BindRequired] int ket, [BindRequired] int filter) => 
        BasicExpression(id, KetExpression.NewFilter(ket, filter));

    /// ----------------
    /// Map expressions
    /// ----------------
    [HttpGet("{id}/bool")]
    [HttpPost("{id}/bool")]
    public ActionResult<int> ConstantBool(string id, [BindRequired] bool value) => 
        BasicExpression(id, KetExpression.NewMap(-1, KetMapOperator.NewConstant(Value.NewBool(value))));

    [HttpGet("{id}/int")]
    [HttpPost("{id}/int")]
    public ActionResult<int> ConstantInt(string id, [BindRequired] int value) => 
        BasicExpression(id, KetExpression.NewMap(-1, KetMapOperator.NewConstant(Value.NewInt(value))));

    [HttpGet("{id}/add")]
    [HttpPost("{id}/add")]
    public ActionResult<int> Add(string id, [BindRequired] int left, [BindRequired] int right) => 
        ExpressionWithArgs(id, new int[] {left, right}, KetMapOperator.Add);
    
    [HttpGet("{id}/multiply")]
    [HttpPost("{id}/multiply")]
    public ActionResult<int> Multiply(string id, [BindRequired] int left, [BindRequired] int right) => 
        ExpressionWithArgs(id, new int[] {left, right}, KetMapOperator.Multiply);
    
    [HttpGet("{id}/and")]
    [HttpPost("{id}/and")]
    public ActionResult<int> And(string id, [BindRequired] int left, [BindRequired] int right) => 
        ExpressionWithArgs(id, new int[] {left, right}, KetMapOperator.And);
    
    [HttpGet("{id}/or")]
    [HttpPost("{id}/or")]
    public ActionResult<int> Or(string id, [BindRequired] int left, [BindRequired] int right) => 
        ExpressionWithArgs(id, new int[] {left, right}, KetMapOperator.Or);
    
    [HttpGet("{id}/eq")]
    [HttpPost("{id}/eq")]
    public ActionResult<int> Eq(string id, [BindRequired] int left, [BindRequired] int right) => 
        ExpressionWithArgs(id, new int[] {left, right}, KetMapOperator.Eq);

    [HttpGet("{id}/not")]
    [HttpPost("{id}/not")]
    public ActionResult<int> Eq(string id, [BindRequired] int ket) => 
        BasicExpression(id, KetExpression.NewMap(ket, KetMapOperator.Not));

    [HttpGet("{id}/lte")]
    [HttpPost("{id}/lte")]
    public ActionResult<int> LetLessThanEquals(string id, [BindRequired] int left, [BindRequired] int right) => 
        ExpressionWithArgs(id, new int[] {left, right}, KetMapOperator.LessThanEqual);
    
    [HttpGet("{id}/gt")]
    [HttpPost("{id}/gt")]
    public ActionResult<int> GreaterThan(string id, [BindRequired] int left, [BindRequired] int right) => 
        ExpressionWithArgs(id, new int[] {left, right}, KetMapOperator.GreaterThan);
    
    [HttpGet("{id}/if")]
    [HttpPost("{id}/if")]
    public ActionResult<int> If(string id, [BindRequired] int cond, [BindRequired] int t, [BindRequired] int e) => 
        ExpressionWithArgs(id, new int[] {cond, t, e}, KetMapOperator.If);
    

    private ActionResult<int> BasicExpression(string id, KetExpression exp) {

        if (_graphs.TryFind(id, out var g)) {
            var (k1, g1) = g.Add(exp);
            _graphs.Update(id, g1);
            return (k1);
        }

        return NotFound();
    }

    private ActionResult<int> ExpressionWithArgs(string id, int[] args, KetMapOperator op) {

        if (_graphs.TryFind(id, out var g)) {
            var (k1, g1) = g.Add(KetExpression.NewJoin(ListModule.OfSeq(args)));
            var (k2, g2) = g1.Add(KetExpression.NewMap(k1, op));
            _graphs.Update(id, g2);
            return k2;
        }

        return NotFound();
    }
}
