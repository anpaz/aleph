
using Newtonsoft.Json;
using static aleph.runtime.Eval;

namespace aleph.server {

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

        private readonly QuantumGraph Graph;

        public GraphNode(int id, QuantumGraph graph) {
            var exp = graph.Item(id);

            this.Graph = graph;
            this.Id = id;
            this.Label = CreateLabel(exp);
            this.Dependencies = CreateDependencies(exp, graph);
        }

        private GraphNode[] CreateDependencies(KetExpression exp, QuantumGraph graph)
        {
            if (exp.IsLiteral) {
                return new GraphNode[0];
            } else if (exp.IsJoin) {
                var ket = (KetExpression.Join) exp;
                return ket.ids.Select(id => new GraphNode(id, graph)).ToArray();
            } else if (exp.IsProject) {
                var ket = (KetExpression.Project) exp;
                return new GraphNode[] { new GraphNode(ket.ket, graph)};
            } else if (exp.IsMap) {
                var ket = (KetExpression.Map) exp;
                return (ket.input < 0)
                    ? new GraphNode[0]
                    : new GraphNode[] { new GraphNode(ket.input, graph)};
            } else if (exp.IsFilter) {
                var ket = (KetExpression.Filter) exp;
                return new GraphNode[] { 
                    new GraphNode(ket.input, graph),
                    new GraphNode(ket.filter, graph),
                };
            }

            throw new NotImplementedException();
        }

        private string CreateLabel(KetExpression exp)
        {
            if (exp.IsLiteral) {
                var ket = (KetExpression.Literal) exp;
                return $"literal (width: {ket.size})";
            } else if (exp.IsJoin) {
                var ket = (KetExpression.Join) exp;
                return $"join";
            } else if (exp.IsProject) {
                var ket = (KetExpression.Project) exp;
                return $"literal (index: {ket.index})";
            } else if (exp.IsMap) {
                var ket = (KetExpression.Map) exp;
                return $"map ({OpLabel(ket.lambda)})";
            } else if (exp.IsFilter) {
                var ket = (KetExpression.Filter) exp;
                return $"filter";
            }

            throw new NotImplementedException();
        }

        private string OpLabel(KetMapOperator op) =>
            op.IsAdd ?
                "add" :
            op.IsAnd ?
                "and" :
            op.IsConstant ?
                $"constant (" + ((KetMapOperator.Constant) op).c.ToString() + ")" :
            op.IsEq ?
                "eq" :
            op.IsGreaterThan ?
                "greater than" :
            op.IsIf ?
                "if" :
            op.IsIn ?
                "in" :
            op.IsLessThanEqual ?
                "less than equal" :
            op.IsMultiply ?
                "multiply" :
            op.IsNot ?
                "not" :
            op.IsOr ?
                "or" :
            throw new NotImplementedException();

        public int Id {get;}

        public string Label { get; }

        public GraphNode[] Dependencies { get; }
    }
}