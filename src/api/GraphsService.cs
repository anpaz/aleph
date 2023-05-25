namespace aleph.server;

using static aleph.kets;


public interface IGraphsService
{
    bool TryFind(string id, out QuantumGraph? value);
    QuantumGraph Add(string id, QuantumGraph graph);
    QuantumGraph Update(string id, QuantumGraph graph);
}

public class GraphsService : IGraphsService
{
    private readonly Dictionary<string, QuantumGraph> _graphs = new Dictionary<string, QuantumGraph>();

    public bool TryFind(string id, out QuantumGraph? value) =>
        _graphs.TryGetValue(id, out value);

    public QuantumGraph Add(string id, QuantumGraph value) =>
        _graphs[id] = value;

    public QuantumGraph Update(string id, QuantumGraph value) =>
        _graphs[id] = value;
}
