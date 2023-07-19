
namespace aleph.server;

using static aleph.kets;

public class QuantumGraph
{
    private static readonly object lockObject = new object();
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
        var id = 0;
        lock (lockObject)
        {
            id = ++this.last;
        }

        nodes.Add(id, new KetValue(id, expression));
        return id;
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
