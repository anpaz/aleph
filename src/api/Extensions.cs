namespace aleph.server;

using Microsoft.Azure.Functions.Worker.Http;
using Microsoft.FSharp.Collections;
using System.Net;
using System.Text.Json;

using static aleph.kets;

public static class HttpExtensions
{
    public static HttpResponseData Ok(this HttpRequestData req, object result)
    {
        var response = req.CreateResponse(HttpStatusCode.OK);

        if (result is string msg) {
            response.Headers.Add("Content-Type", "text/plain; charset=utf-8");
            response.WriteString(msg);
        } else if (result is int id) {
            response.Headers.Add("Content-Type", "text/plain; charset=utf-8");
            response.WriteString(id.ToString());
        } else {
            response.Headers.Add("Content-Type", "application/json; charset=utf-8");
            var options = new JsonSerializerOptions
            {
                PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
                WriteIndented = true
            };
            string json = JsonSerializer.Serialize(result, options);
            response.WriteString(json);
        }

        return response;
    }

    public static HttpResponseData NotFound(this HttpRequestData req)
    {
        var response = req.CreateResponse(HttpStatusCode.NotFound);
        return response;
    }

    public static HttpResponseData Run(this HttpRequestData req, IGraphsService graphs, string graphId, Func<QuantumGraph, object> lambda)
    {
        if (graphs.TryFind(graphId, out var g) && g is not null)
        {
            return Ok(req, lambda(g));
        }

        return NotFound(req);
    }

    public static PrepareContext GetQuantumContext(this HttpRequestData req)
    {
        string? backend = req.Query.AllKeys.FirstOrDefault(key => key == "backend");

        if (backend is null)
        {
            return new PrepareContext(new aleph.qpu.classic.Processor());
        }
        else
        {
            var value = req.Query[backend];
            if (value == "quantum" || value == "qsharp" || value == "sparsesimulator")
            {
                return new PrepareContext(new aleph.qpu.qsharp.Processor(aleph.qpu.qsharp.context.simulator));
            }
            else if (value == "classic")
            {
                return new PrepareContext(new aleph.qpu.classic.Processor());
            }
        }

        throw new ArgumentException("backend");
    }
}

public static class KetValueExtensions
{
    public static string Label(this KetValue ket)
    {
        if (ket.Expression.IsLiteral)
        {
            var literal = (KetExpression.Literal)ket.Expression;
            return $"literal (width: {literal.width})";
        }
        else if (ket.Expression.IsConstant)
        {
            var c = (KetExpression.Constant)ket.Expression;
            return $"constant ({c.value})";
        }
        else if (ket.Expression.IsMap)
        {
            var map = (KetExpression.Map)ket.Expression;
            return $"map ({map.op.Label()})";
        }
        else if (ket.Expression.IsWhere)
        {
            var w = (KetExpression.Where)ket.Expression;
            return $"where (op: {w.clause.Label()})";
        }

        throw new NotImplementedException();
    }

    public static GraphNode[] Dependencies(this KetValue ket, QuantumGraph graph)
    {
        if (ket.Expression.IsLiteral || ket.Expression.IsConstant)
        {
            return new GraphNode[0];
        }
        else if (ket.Expression.IsMap)
        {
            var map = (KetExpression.Map)ket.Expression;
            return map.args
                .Select((ket, idx) => new GraphNode(ket.Id, graph))
                .ToArray();
        }
        else if (ket.Expression.IsWhere)
        {
            var where = (KetExpression.Where)ket.Expression;
            return where.args
                .Select((ket, idx) => new GraphNode(ket.Id, graph))
                .Append(new GraphNode(where.target.Id, graph))
                .ToArray();
        }

        throw new NotImplementedException();
    }
}

public static class OperatorExtensions
{
    public static string Label(this Operator op) =>
            op.IsNot ?
                "not" :
            op.IsLessThanEquals ?
                "leq" :
            op.IsEq ?
                "eq" :
            op.IsIf ?
                "if" :
            op.IsId ?
                "id" :
            op.IsGreaterThan ?
                ">" :
            op.IsAnd ?
                "and" :
            op.IsOr ?
                "or" :
            op.IsAdd ?
                $"add_{((Operator.Add)op).width}" :
            op.IsMultiply ?
                $"multiply_{((Operator.Multiply) op).width}" :
            op.IsIn ?
                "in:" + String.Join(',', ((Operator.In) op).values) :
            throw new NotImplementedException();

    public static Operator Parse(string label)
    {
        if ("eq" == label) {
            return Operator.Eq;
        } if ("not" == label) {
            return Operator.Not;
        } if ("lte" == label) {
            return Operator.LessThanEquals;
        } if ("if" == label) {
            return Operator.If;
        } if ("id" == label) {
            return Operator.Id;
        } if ("gt" == label) {
            return Operator.GreaterThan;
        } if ("and" == label) {
            return Operator.And;
        } if ("or" == label) {
            return Operator.Or;
        } if (label.StartsWith("add_")) {
            var width = int.Parse(label.Substring("add_".Length));
            return Operator.NewAdd(width);
        } if (label.StartsWith("multiply_")) {
            var width = int.Parse(label.Substring("multiply_".Length));
            return Operator.NewMultiply(width);
        } if (label.StartsWith("in:")) {
            var values = label.Substring("in:".Length).Split(',').Select(i => int.Parse(i));
            return Operator.NewIn(ListModule.OfSeq(values));
        }

        throw new NotImplementedException();
    }
}
