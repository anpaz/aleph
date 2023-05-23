namespace aleph.server

open System
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging

open aleph.kets
open aleph.utils
open Utils

[<ApiController>]
[<Route("[controller]")>]
type GraphController (logger : ILogger<GraphController>, graphs: IGraphsService) =
    inherit ControllerBase()

    let getKets (graph: QuantumGraph) (ids: string) =
        ids.Split(',')
        |> Array.map (fun id -> id |> int)
        |> Array.map (fun k -> graph[k])
        |> Array.toList

    [<HttpGet>]
    member this.Get() =
        ":ℵ-0.9:" |> this.Ok

    [<HttpGet("~create")>]
    [<HttpPost("")>]
    member _.Create() =
        let id = Guid.NewGuid().ToString("D")
        let g = QuantumGraph()
        graphs.Add(id, g) |> ignore
        id

    [<Route("{graphId}")>]
    member this.GetGraph(graphId: string) =
        match graphs.TryFind(graphId) with
        | Some g -> g |> this.Ok :> IActionResult
        | _ -> this.NotFound()

    [<Route("{graphId}/{ketid}")>]
    member this.GetNode(graphId: string, ketId: int) : IActionResult =
        match graphs.TryFind(graphId) with
        | Some graph -> this.Ok(GraphNode(ketId, graph))
        | _ -> this.NotFound()

    [<Route("{graphId}/literal")>]
    member this.Literal(graphId: string, width: int) =
        this.AddExpression(graphId, Expression.Literal width)

    [<Route("{graphId}/bool")>]
    member this.ConstantBool(graphId: string, value: bool) =
        this.AddExpression(graphId, Expression.Constant (if value then 1 else 0))

    [<Route("{graphId}/int")>]
    member this.ConstantInt(graphId: string, value: int) =
        this.AddExpression(graphId, Expression.Constant value)

    [<Route("{graphId}/where")>]
    member this.Where(graphId: string, id: int, op: string, arg: int) =
        this.AddWhereExpression(graphId, id, Operator.Parse(op), arg)

    [<Route("{graphId}/where-in")>]
    member this.WhereIn(graphId: string, id: int, values: string) =
        this.AddWhereExpression(graphId, id, Operator.In (values.Split(',') |> Array.map int |> Array.toList), id)

    [<Route("{graphId}/map/not")>]
    member this.Not(graphId: string, id: int) =
        this.AddMapExpression(graphId, Operator.Not, [ id ])

    [<Route("{graphId}/map/lte")>]
    member this.LessThanEquals(graphId: string, left: int, right: int) =
        this.AddMapExpression(graphId, Operator.LessThanEquals, [ left; right ])

    [<Route("{graphId}/map/eq")>]
    member this.Eq(graphId: string, left: int, right: int) =
        this.AddMapExpression(graphId, Operator.Eq, [ left; right ])

    [<Route("{graphId}/map/add")>]
    member this.Add(graphId: string, left: int, right: int, width: int) =
        this.AddMapExpression(graphId, Operator.Add width, [ left; right ])

    [<Route("{graphId}/map/multiply")>]
    member this.Multiply(graphId: string, left: int, right: int, width: int) =
        this.AddMapExpression(graphId, Operator.Multiply width, [ left; right ])

    [<Route("{graphId}/map/id")>]
    member this.Id(graphId: string, id: int) =
        this.AddMapExpression(graphId, Operator.Id, [ id ])

    [<Route("{graphId}/map/in")>]
    member this.In(graphId: string, id: int, [<FromQuery>] value: int[]) =
        this.AddMapExpression(graphId, Operator.In (value |> Array.toList), [ id ])

    [<Route("{graphId}/map/gt")>]
    member this.GreaterThan(graphId: string, left: int, right: int) =
        this.AddMapExpression(graphId, Operator.GreaterThan, [ left; right ])

    [<Route("{graphId}/map/and")>]
    member this.And(graphId: string, left: int, right: int) =
        this.AddMapExpression(graphId, Operator.And, [ left; right ])

    [<Route("{graphId}/map/or")>]
    member this.Or(graphId: string, left: int, right: int) =
        this.AddMapExpression(graphId, Operator.Or, [ left; right ])

    member this.AddExpression(graphId: string, expression: Expression) =
        match graphs.TryFind(graphId) with
        | Some graph -> graph.Add(expression) |> this.Ok :> IActionResult
        | _ -> this.NotFound()

    member private this.AddWhereExpression(graphId: string, id: int, op: Operator, arg: int) =
        match graphs.TryFind(graphId) with
        | Some graph ->
            let target = graph.[id]
            let args = [ graph.[arg] ]
            let expression = Expression.Where(target, op, args)
            graph.Add(expression) |> this.Ok :> IActionResult
        | _ -> this.NotFound()

    member private this.AddMapExpression(graphId: string, op: Operator, argIds: int list) =
        match graphs.TryFind(graphId) with
        | Some graph ->
            let kets = argIds |> List.map (fun k -> graph.[k])
            let expression = Expression.Map(op, kets)
            graph.Add(expression) |> this.Ok :> IActionResult
        | _ -> this.NotFound()

    [<Route("{graphId}/~sample")>]
    member this.Sample(graphId: string, ids: string, filter: int) = //, backend: Backend option
        match graphs.TryFind(graphId) with
        | Some graph ->
            let ctx = this.getQuantumContext()
            let kets = getKets graph ids

            let result =
                if filter > 0 then
                    sample_when ctx (kets, graph.[filter])
                else
                    sample ctx kets

            match result with
            | Ok value -> value |> this.Ok :> IActionResult
            | _ -> new StatusCodeResult(500) :> IActionResult
        | _ -> this.NotFound() :> IActionResult

    [<Route("{graphId}/~prepare")>]
    member this.Prepare(graphId: string, ids: string, filter: int) =
        match graphs.TryFind(graphId) with
        | Some graph ->
            let ctx = this.getQuantumContext()
            let kets = getKets graph ids

            let result =
                if filter > 0 then
                    prepare_when ctx (kets, graph.[filter])
                else
                    prepare ctx kets

            match result with
            | Ok value -> value |> this.Ok :> IActionResult
            | _ -> new StatusCodeResult(500) :> IActionResult
        | _ -> this.NotFound() :> IActionResult


    [<Route("{graphId}/~histogram")>]
    member this.Histogram(graphId: string, ids: string, filter: int) : IActionResult=
        match graphs.TryFind(graphId) with
        | Some graph ->
            let prep = this.Prepare(graphId, ids, filter)
            match prep with
            | :? OkObjectResult as okResult ->
                let universe = okResult.Value :?> IUniverse
                let kets = getKets graph ids
                
                match universe.Histogram(kets) with
                | Result.Ok h -> 
                    let map_key (key: int list) = key |> List.map string |> String.concat ","
                    let map_values (m: Map<string, int>) key value = m.Add(map_key key, value)
                    h |> Map.fold map_values Map.empty |> this.Ok :> IActionResult
                | Result.Error _ -> new StatusCodeResult(500)
            | _ -> prep
        | _ -> this.NotFound() :> IActionResult


    member private this.getQuantumContext() = 
        if this.Request.Query.Keys.Contains("backend") then
            let b = this.Request.Query["backend"].Item 0
            match b.ToLowerInvariant() with
            | "classic" -> 
                { qpu = aleph.qpu.classic.Processor() }
            | "quantum" | "sparsesimulator" ->
                { qpu = aleph.qpu.qsharp.Processor(aleph.qpu.qsharp.context.simulator) }
            | _ -> failwith $"Invalid backend: {b}"
        else 
            { qpu = aleph.qpu.classic.Processor() }