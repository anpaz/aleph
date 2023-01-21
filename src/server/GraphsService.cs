
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
}