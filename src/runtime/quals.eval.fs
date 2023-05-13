namespace aleph.quals.runtime

open aleph.quals.parser.ast

module Eval =

    type KetId = Expression

    type Node =
        | Ket of weight: int
        | Constant of value:int
        | Map of operator:Operator * args:Node list
        | Where of target:Node * clause : Operator * args:Node list

    and QuantumGraph(q: Map<KetId, Node>, max_ket: int) =

        static member empty: QuantumGraph = QuantumGraph (Map.empty, 0)
        member self.Item(k: KetId) : Node = q.Item k
        // member self.TryFind(k: KetId) = q.TryFind k

        member self.Add(v: Node) : (KetId * QuantumGraph) =
            let k = max_ket + 1
            k, QuantumGraph(q.Add(k, v), k)

    type EvalContext = {
        graph: QuantumGraph
    }

    type Ket = 
        | KetExpression of Expression

    let eval ctx (ket: Expression) : (KetId * QuantumGraph) =
        let graph = ctx.graph

        match ket with
        | Expression.Ket w ->
            graph.Add (Ket w)
        | Expression.Constant v ->
            graph.Add (Constant v)
        | _ ->
            failwith $"Not implemented: {ket}"

