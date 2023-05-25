using System.Net;
using Microsoft.Azure.Functions.Worker;
using Microsoft.Azure.Functions.Worker.Http;
using Microsoft.Extensions.Logging;

using System.Text.Json;
using System.Text.Json.Serialization;

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

        private HttpResponseData Ok(object result, HttpRequestData req) {
            var response = req.CreateResponse(HttpStatusCode.OK);
            response.Headers.Add("Content-Type", "application/json; charset=utf-8");

            string msg = JsonSerializer.Serialize(result);
            response.WriteString(msg);
            return response;
        }


        private HttpResponseData NotFound(HttpRequestData req) {
            var response = req.CreateResponse(HttpStatusCode.NotFound);
            return response;
        }

        [Function("Create")]
        public HttpResponseData Create([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/~create")] HttpRequestData req)
        {
            var id = Guid.NewGuid().ToString("D");
            var g = new QuantumGraph();
            _graphs.Add(id, g);

            _logger.LogInformation($"Created quantum graph: {id}");
            
            return Ok(id, req);
        }

        [Function("GetGraph")]
        public HttpResponseData GetGraph([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}")] HttpRequestData req,
            string graphId)
        {
            var msg = $"Fetch graphid: {graphId}";
            if (_graphs.TryFind(graphId, out var g) && g is not null)
            {
                return Ok(g, req);
            }

            return NotFound(req);
        }
        
        [Function("GetNode")]
        public HttpResponseData GetNode([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}/node/{ketId}")] HttpRequestData req,
            string graphId, int ketId)
        {
            if (_graphs.TryFind(graphId, out var graph) && graph is not null)
            {
                return Ok(new GraphNode(ketId, graph), req);
            }

            return NotFound(req);
        }
        
        [Function("Literal")]
        public HttpResponseData Literal([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}/~literal")] HttpRequestData req,
            string graphId, int width) =>
            AddExpression(graphId, Expression.NewLiteral(width), req);

        private HttpResponseData AddExpression(string graphId, Expression expression, HttpRequestData req)
        {
            if (_graphs.TryFind(graphId, out var graph) && graph is not null)
            {
                return Ok(graph.Add(expression), req);
            }

            return NotFound(req);
        }
    }
}
