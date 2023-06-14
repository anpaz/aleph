using Microsoft.Azure.Functions.Worker;
using Microsoft.Azure.Functions.Worker.Http;
using Microsoft.Extensions.Logging;
using Microsoft.FSharp.Collections;

using static aleph.kets;

namespace aleph.server
{
    public class GraphFunctions
    {
        private readonly ILogger _logger;
        private readonly IGraphsService _graphs;

        public GraphFunctions(ILoggerFactory loggerFactory, IGraphsService graphs)
        {
            _logger = loggerFactory.CreateLogger<GraphFunctions>();
            _graphs = graphs;
        }

        [Function("Create")]
        public HttpResponseData Create([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/~create")] HttpRequestData req)
        {
            var id = Guid.NewGuid().ToString("D");
            var g = new QuantumGraph();
            _graphs.Add(id, g);

            _logger.LogInformation($"Created quantum graph: {id}");
            
            return req.Ok(id);
        }

        [Function("GetGraph")]
        public HttpResponseData GetGraph([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}")] HttpRequestData req,
            string graphId) => req.Run(_graphs, graphId, graph => graph);

        [Function("GetNode")]
        public HttpResponseData GetNode([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}/node/{ketId}")] HttpRequestData req,
            string graphId, int ketId) => req.Run(_graphs, graphId, graph => new GraphNode(ketId, graph));
        
        [Function("Literal")]
        public HttpResponseData Literal([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}/~literal")] HttpRequestData req,
            string graphId, int width) => req.Run(_graphs, graphId, graph => graph.Add(Expression.NewLiteral(width)));

        [Function("ConstantBool")]
        public HttpResponseData ConstantBool([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}/~bool")] HttpRequestData req,
            string graphId, string value) => req.Run(_graphs, graphId, graph => graph.Add(Expression.NewConstant(bool.Parse(value) ? 1 : 0)));

        [Function("ConstantInt")]
        public HttpResponseData ConstantInt([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}/~int")] HttpRequestData req,
            string graphId, int value) => req.Run(_graphs, graphId, graph => graph.Add(Expression.NewConstant(value)));

        [Function("Where")]
        public HttpResponseData Where([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}/~where")] HttpRequestData req,
            string graphId, int id, string op, string args) => req.Run(_graphs, graphId, graph =>
        {
            var target = graph[id];
            var kets = ListModule.OfSeq(GetKets(graph, args));
            var clause = OperatorExtensions.Parse(op);
            var expression = Expression.NewWhere(target, clause, kets);
            return graph.Add(expression);
        });

        [Function("MapNot")]
        public HttpResponseData MapNot([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}/~map/not")] HttpRequestData req,
            string graphId, int id) => req.Run(_graphs, graphId, graph => 
        AddMapExpression(graph, Operator.Not, new int[] { id }));
        
        [Function("MapId")]
        public HttpResponseData MapId([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}/~map/id")] HttpRequestData req,
            string graphId, int id) => req.Run(_graphs, graphId, graph => 
        AddMapExpression(graph, Operator.Id, new int[] { id }));

        [Function("MapIf")]
        public HttpResponseData MapIf([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}/~map/if")] HttpRequestData req,
            string graphId, int cond, int t, int f) => req.Run(_graphs, graphId, graph => 
        AddMapExpression(graph, Operator.If, new int[] { cond, t, f }));

        [Function("MapOther")]
        public HttpResponseData Map([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}/~map/{op}")] HttpRequestData req,
            string graphId, string op, int left, int right) => req.Run(_graphs, graphId, graph => 
        AddMapExpression(graph, OperatorExtensions.Parse(op), new int[] { left, right }));

        [Function("Sample")]
        public HttpResponseData Sample([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}/~sample")] HttpRequestData req,
            string graphId, string ids, int filter) => req.Run(_graphs, graphId, graph =>
        {
            var ctx = req.GetQuantumContext();
            var kets = ListModule.OfSeq(GetKets(graph, ids));

            var result = filter > 0
                    ? sample_when(ctx, kets, graph[filter])
                    : sample(ctx, kets);

            if (result.IsOk)
            {
                return result.ResultValue.ToArray();
            }
            else
            {
                throw new InvalidOperationException(result.ErrorValue);
            }
        });

        [Function("Prepare")]
        public HttpResponseData Prepare([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}/~prepare")] HttpRequestData req,
            string graphId, string ids, int filter) => req.Run(_graphs, graphId, graph =>
        {
            var ctx = req.GetQuantumContext();
            var kets = ListModule.OfSeq(GetKets(graph, ids));

            var result = filter > 0
                    ? prepare_when(ctx, kets, graph[filter])
                    : prepare(ctx, kets);

            if (result.IsOk)
            {
                return result.ResultValue;
            }
            else
            {
                throw new InvalidOperationException(result.ErrorValue);
            }
        });
        
        [Function("Histogram")]
        public HttpResponseData Histogram([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}/~histogram")] HttpRequestData req,
            string graphId, string ids, int rounds, int filter) => req.Run(_graphs, graphId, graph =>
        {
            var ctx = req.GetQuantumContext();
            var kets = ListModule.OfSeq(GetKets(graph, ids));

            var result = filter > 0
                    ? prepare_when(ctx, kets, graph[filter])
                    : prepare(ctx, kets);

            if (result.IsOk)
            {
                var histogram = result.ResultValue.Histogram(kets, rounds);
                var map = new Dictionary<string, int>();
                foreach (var item in histogram.ResultValue) {
                    var key = "[" + String.Join(',', item.Key.Select(key => key.ToString())) + "]";
                    var count = item.Value;
                    map.Add(key, count);
                }
                return  map;
            }
            else
            {
                throw new InvalidOperationException(result.ErrorValue);
            }
        });

        private static int AddMapExpression(QuantumGraph graph, Operator op, int[] argIds)
        {
            var kets = ListModule.OfSeq(argIds.Select((k, idx) => graph[k]));
            var expression = Expression.NewMap(op, kets);
            return graph.Add(expression);
        }

        private static IEnumerable<KetValue> GetKets(QuantumGraph graph, string ids) =>
            ids.Split(',')
            .Select(id => int.Parse(id))
            .Where(id => id > 0)
            .Select(k => graph[k]);

    }
}
