using System.Net;
using Microsoft.Azure.Functions.Worker;
using Microsoft.Azure.Functions.Worker.Http;
using Microsoft.Extensions.Logging;

using System.Text.Json;
using System.Text.Json.Serialization;
using System.Linq;

using static aleph.kets;

using System.Diagnostics.Metrics;
using System.Runtime.InteropServices;
using Azure.Core;
using Microsoft.FSharp.Collections;

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


        [Function("Sample")]
        public HttpResponseData Sample([HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "graph/{graphId}/~sample")] HttpRequestData req,
            string graphId, string ids, int filter) => req.Run(_graphs, graphId, graph =>
        {
            var ctx = req.GetQuantumContext();
            var kets = ListModule.OfArray(GetKets(graph, ids));

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
            var kets = ListModule.OfArray(GetKets(graph, ids));

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

        private static KetValue[] GetKets(QuantumGraph graph, string ids) =>
            ids.Split(',')
            .Select(id => int.Parse(id))
            .Select(k => graph[k])
            .ToArray();

    }
}
