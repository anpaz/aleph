
namespace aleph.server;

using static aleph.kets;

public class QuantumGraph
{
    private int last = 0;
    private readonly Dictionary<int, KetValue> nodes = new Dictionary<int, KetValue>();

    public KetValue this[int id]
    {
        get
        {
            return nodes[id];
        }
    }

    public int Add(KetExpression expression)
    {
        this.last++;
        nodes.Add(last, new KetValue(this.last, expression));
        return this.last;
    }
}

public class GraphNode
{
    public GraphNode(int id, QuantumGraph graph)
    {
        var ket = graph[id];

        this.Id = id;
        this.Label = ket.Label();
        this.Dependencies = ket.Dependencies(graph);
    }

    public int Id { get; }

    public string Label { get; }

    public GraphNode[] Dependencies { get; }
}
