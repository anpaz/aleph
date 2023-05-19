
using Newtonsoft.Json;
using static aleph.kets;

namespace aleph.server {

    public class QuantumGraph {
        private int last = 0;
        private readonly Dictionary<int, KetValue> nodes = new Dictionary<int, KetValue>();

        public KetValue this[int id] {
            get {
                return nodes[id];
            }
        }

        public int Add(Expression expression) {
            this.last++;
            nodes.Add(last, new KetValue(expression, this.last));
            return this.last;
        }
    }

    public interface IGraphsService {
        bool TryFind(string id, out QuantumGraph value);
        QuantumGraph Add(string id, QuantumGraph graph);
        QuantumGraph Update(string id, QuantumGraph graph);
    }

    public class GraphsService : IGraphsService
    {
        private readonly Dictionary<string, QuantumGraph> _graphs = new Dictionary<string, QuantumGraph>();

        public bool TryFind(string id, out QuantumGraph value) =>
#pragma warning disable 8601 //Possible null reference assignment, by semantics this is not possible
            _graphs.TryGetValue(id, out value);
#pragma warning restore 8601

        public QuantumGraph Add(string id, QuantumGraph value) =>
            _graphs[id] = value;

        public QuantumGraph Update(string id, QuantumGraph value) =>
            _graphs[id] = value;
    }

    public class GraphNode {

        public GraphNode(int id, QuantumGraph graph) {
            var ket = graph[id];

            this.Id = id;
            this.Label = CreateLabel(ket);
            this.Dependencies = CreateDependencies(ket, graph);
        }

        private GraphNode[] CreateDependencies(KetValue ket, QuantumGraph graph)
        {
            if (ket.Expression.IsLiteral || ket.Expression.IsConstant) {
                return new GraphNode[0];
            } else if (ket.Expression.IsMap) {
                var map = (Expression.Map) ket.Expression;
                return map.args
                    .Select((ket, idx) =>  new GraphNode(ket.Id, graph))
                    .ToArray();
            } else if (ket.Expression.IsWhere) {
                var where = (Expression.Where) ket.Expression;
                return where.args
                    .Select((ket, idx) =>  new GraphNode(ket.Id, graph))
                    .Append(new GraphNode(where.target.Id, graph))
                    .ToArray();
            }

            throw new NotImplementedException();
        }

        private string CreateLabel(KetValue ket)
        {
            if (ket.Expression.IsLiteral) {
                var literal = (Expression.Literal) ket.Expression;
                return $"literal (width: {literal.width})";
            } else if (ket.Expression.IsConstant) {
                var c = (Expression.Constant) ket.Expression;
                return $"constant (width: {c.value})";
            } else if (ket.Expression.IsMap) {
                var map = (Expression.Map) ket.Expression;
                return $"map ({OpLabel(map.op)})";
            } else if (ket.Expression.IsWhere) {
                var w = (Expression.Where) ket.Expression;
                return $"where (op: {OpLabel(w.clause)})";
            }

            
            throw new NotImplementedException();
        }

        private string OpLabel(Operator op) =>
            op.IsNot ?
                "not" :
            op.IsLessThanEquals ?
                "less than equal" :
            op.IsEq ?
                "eq" :
            op.IsAdd ?
                $"add, width={((Operator.Add) op).width}" :
            op.IsMultiply ?
                "multiply, width={((Operator.Multiply) op).width}" :
            op.IsIf ?
                "if" :
            op.IsId ?
                "id" :
            op.IsIn ?
                "in" :
            op.IsGreaterThan ?
                "greater than" :
            op.IsAnd ?
                "and" :
            op.IsOr ?
                "or" :
            throw new NotImplementedException();

        public int Id {get;}

        public string Label { get; }

        public GraphNode[] Dependencies { get; }
    }
}