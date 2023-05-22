namespace aleph.server

open aleph.kets
open Utils

type QuantumGraph() =
    let mutable last = 0
    let mutable nodes = Map.empty
    member this.Item 
        with get(id) = nodes.[id]

    member this.Add(expression) =
        last <- last + 1
        nodes <- nodes.Add(last, KetValue(expression, last))
        last

    member this.Count = last

type IGraphsService =
    abstract member TryFind : id: string -> Option<QuantumGraph>
    abstract member Add : id: string * value: QuantumGraph -> QuantumGraph
    abstract member Update : id: string * value: QuantumGraph -> QuantumGraph

type GraphsService() =
    let mutable graphs = Map.empty

    interface IGraphsService with
        member _.TryFind(id) = graphs.TryFind(id)
        member _.Add(id, value) = 
            graphs <- graphs.Add(id, value)
            value
        member _.Update(id, value) =
            graphs <- graphs.Add(id, value)
            value

type GraphNode(id: int, graph: QuantumGraph) =
    let ket = graph.[id]
    let dependencies (ket: KetValue) graph =
        match ket.Expression with
        | Expression.Literal _
        | Expression.Constant _ -> Seq.empty
        | Expression.Map (op, args) -> 
            args |> Seq.map (fun ket -> GraphNode(ket.Id, graph))
        | Expression.Where (target, op, args) -> 
            args 
            |> Seq.map (fun ket -> GraphNode(ket.Id, graph)) 
            |> Seq.append [GraphNode(target.Id, graph)]


    member val Id = id
    member val Label = ket.Label()
    member val Dependencies = dependencies ket graph
