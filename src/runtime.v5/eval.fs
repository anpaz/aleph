namespace aleph.runtime

open aleph.parser.ast
open aleph.parser.ast.typed

module EvalV5 =
    let random = System.Random()
    
    let mutable max_ket = 0
    let fresh_ketid () =
        max_ket <- max_ket + 1
        max_ket


    let (==>) (input: Result<'a, 'b>) ok = Result.bind ok input

    type IUniverse =
        interface
            inherit System.IComparable
        end

    and [<CustomComparison; CustomEquality>] Method =
        { Args: Id list
          Body: E
          Context: EvalContext }

        override this.Equals x =
            match x with
            | :? Method as { Args = args'; Body = body' } -> (this.Args = args' && this.Body = body')
            | _ -> false

        override this.GetHashCode() =
            this.Args.GetHashCode() + this.Body.GetHashCode() + this.Context.GetHashCode()

        interface System.IComparable with
            member this.CompareTo(obj: obj) : int = failwith "Not Implemented"

    and KetId = int

        // static member Join(ketIds: KetId list) =
        //     let rec unwrap ids =
        //         match ids with 
        //         | head :: tail ->
        //             match head with
        //             | One l -> l :: (unwrap tail)
        //             | Many l -> l @ (unwrap tail)
        //         | [] -> []
        //     Many (unwrap ketIds)

    and Value =
        | Bool of bool
        | Int of int
        | Tuple of Value list
        | Set of Set<Value>
        | Method of Method
        | KetId of KetId
        | Universe of IUniverse

        static member (+)(l: Value, r: Value) =
            match (l, r) with
            | Value.Int l, Value.Int r -> Value.Int(l + r)
            | _ -> failwith "+ only supported for ints, got {l} + {r}"

        static member (*)(l: Value, r: Value) =
            match (l, r) with
            | Value.Int l, Value.Int r -> Value.Int(l * r)
            | _ -> failwith "+ only supported for ints, got {l} * {r}"

        static member (==)(l: Value, r: Value) =
            match (l, r) with
            | Value.Int l, Value.Int r -> Value.Bool(l = r)
            | _ -> failwith "= only supported for ints, got {l} == {r}"

        static member LessThan(l: Value, r: Value) =
            match (l, r) with
            | Value.Int l, Value.Int r -> Value.Bool(l < r)
            | _ -> failwith "< only supported for ints, got {l} == {r}"

        static member Not(l: Value) =
            match l with
            | Value.Bool b -> Value.Bool(not b)
            | _ -> failwith "not only supported for bool values, got {l}"

        static member And(l: Value, r: Value) =
            match (l, r) with
            | Value.Bool l, Value.Bool r -> Value.Bool(l && r)
            | _ -> failwith "= only supported for bool values, got {l} && {r}"

        static member Or(l: Value, r: Value) =
            match (l, r) with
            | Value.Bool l, Value.Bool r -> Value.Bool(l || r)
            | _ -> failwith "= only supported for bool values, got {l} || {r}"

    and QuantumGraph(q: Map<KetId, KetExpression>) =
        static member empty: QuantumGraph = QuantumGraph Map.empty
        member self.Add (k: KetId, v: KetExpression) : QuantumGraph = QuantumGraph(q.Add(k, v))
        member self.TryFind (k: KetId) = q.TryFind k
        member self.Item (k: KetId) : KetExpression = q.Item k


    and KetMapOperator =
        | Constant of c: Value
        | Add
        | Multiply
        | Equals
        | LessThan
        | Not
        | And
        | Or
        | If
        | In of c: Value

    and KetExpression =
        | Literal of size: int
        | Join of ids: KetId list
        | Project of ket: KetId * index: int
        | Map of input: KetId * lambda: KetMapOperator
        | Filter of input: KetId * filter: KetId

    and EvalContext =
        { heap: Map<Id, Value>
          graph: QuantumGraph
          qpu: QPU }

    and QPU =
        abstract Prepare: U * EvalContext -> Result<Value * QuantumGraph, string>
        abstract Measure: IUniverse * EvalContext -> Result<Value * QuantumGraph, string>

    let width (s: Set<Value>) =
        if s.IsEmpty then
            0
        else
            let first = s |> Set.toSeq |> Seq.take 1 |> Seq.toList |> List.head

            match first with
            | Int _
            | Bool _ -> 1
            | Tuple t -> t.Length
            | _ -> 0

    let rec eval_quantum ctx e : Result<Value * QuantumGraph, string> =
        match e with
        | Q.Var id -> eval_var ctx id
        | Q.Ket value -> eval_ket ctx value
        | Q.KetAll size -> eval_qliteral ctx size

        | Q.Constant value -> eval_qmap_constant ctx value
        | Q.Not q -> eval_qmap_unary ctx (q, KetMapOperator.Not)
        | Q.Equals(left, right) -> eval_qmap_binary ctx (left, right, KetMapOperator.Equals)
        | Q.Add(left, right) -> eval_qmap_binary ctx (left, right, KetMapOperator.Add)
        | Q.Multiply(left, right) -> eval_qmap_binary ctx (left, right, KetMapOperator.Multiply)
        | Q.And(left, right) -> eval_qmap_binary ctx (left, right, KetMapOperator.And)
        | Q.Or(left, right) -> eval_qmap_binary ctx (left, right, KetMapOperator.Or)

        | Q.Join(left, right) -> eval_qjoin ctx (left, right)
        | Q.Project(ket, index) -> eval_qproject ctx (ket, index)
        | Q.Index(ket, index) -> eval_qindex ctx (ket, index)

        | Q.IfQuantum(condition, then_q, else_q) -> eval_qif ctx (condition, then_q, else_q)
        | Q.IfClassic(condition, then_q, else_q) -> eval_qif_classic ctx (condition, then_q, else_q)

        | Q.Filter(ket, condition, hint) -> eval_qfilter ctx (ket, condition)

        | Q.Block(stmts, value) -> eval_qblock ctx (stmts, value)
        | Q.CallMethod(method, args) -> eval_callmethod ctx (method, args)

    and with_value (exp: KetExpression) (graph: QuantumGraph) =
        let k = fresh_ketid ()
        (KetId k, graph.Add(k, exp)) |> Ok

    and eval_qliteral ctx size =
        eval_classic ctx size
        ==> fun (value, graph) ->
                match value with
                | Value.Int n -> graph |> with_value (KetExpression.Literal n)
                | _ -> $"Invalid KetAll size: {value}" |> Error

    and eval_qjoin ctx (left, right) =
        eval_quantum ctx left
        ==> fun (value1, q1) ->
                eval_quantum { ctx with graph = q1 } right
                ==> fun (value2, q2) ->
                        match (value1, value2) with
                        | Value.KetId k1, Value.KetId k2 -> q2 |> with_value (KetExpression.Join [k1; k2])
                        | _ -> $"Invalid KetIds" |> Error

    and eval_qproject ctx (ket, index) =
        eval_quantum ctx ket
        ==> fun (value, graph) ->
                match value with
                | Value.KetId k -> graph |> with_value (KetExpression.Project (k, index))
                | _ -> $"Invalid KetIds" |> Error

    and eval_qindex ctx (ket, index) =
        eval_classic ctx index
        ==> fun (index, q1) ->
                match index with
                | Value.Int i ->
                    eval_qproject { ctx with graph = q1 } (ket, i)
                | _ -> $"Invalid KetIds" |> Error

    and eval_qmap_constant ctx value =
        eval_classic ctx value
        ==> fun (v, graph) -> graph |> with_value (KetExpression.Map(-1, KetMapOperator.Constant v))

    and eval_qmap_unary ctx (left, lambda) =
        eval_quantum ctx left
        ==> fun (value, graph) ->
                match value with
                | Value.KetId k1 -> graph |> with_value (KetExpression.Map(k1, lambda))
                | _ -> $"Invalid KetIds" |> Error

    and eval_qmap_binary ctx (left, right, lambda) =
        eval_quantum ctx left
        ==> fun (value1, q1) ->
                eval_quantum { ctx with graph = q1 } right
                ==> fun (value2, q2) ->
                        match (value1, value2) with
                        | Value.KetId(k1), Value.KetId(k2) ->
                            let k = fresh_ketid()
                            q2.Add(k, KetExpression.Join [k1; k2])
                            |> with_value (KetExpression.Map(k, lambda))
                        | _ -> $"Invalid KetIds" |> Error


    and eval_qif ctx (condition, then_q, else_q) =
        eval_quantum ctx condition
        ==> fun (value1, q1) ->
                eval_quantum { ctx with graph = q1 } then_q
                ==> fun (value2, q2) ->
                        eval_quantum { ctx with graph = q2 } else_q
                        ==> fun (value3, q3) ->
                                match (value1, value2, value3) with
                                | Value.KetId k1, Value.KetId k2, Value.KetId k3 ->
                                    let k = fresh_ketid()
                                    q3.Add(k, KetExpression.Join [k1; k2; k3])
                                    |> with_value (KetExpression.Map(k, KetMapOperator.If))
                                | _ -> $"Invalid KetIds for if expression" |> Error

    and eval_qif_classic ctx (condition, then_q, else_q) =
        eval_classic ctx condition
        ==> fun (v1, q1) ->
                match v1 with
                | Bool true -> eval_quantum { ctx with graph = q1 } then_q ==> fun (v2, q2) -> (v2, q2) |> Ok
                | Bool false -> eval_quantum { ctx with graph = q1 } else_q ==> fun (v3, q3) -> (v3, q3) |> Ok
                | _ -> $"Invalid if condition. Expecting boolean value, got {v1}" |> Error

    and eval_qfilter ctx (ket, condition) =
        eval_quantum ctx ket
        ==> fun (value1, q1) ->
                eval_quantum { ctx with graph = q1 } condition
                ==> fun (value2, q2) ->
                        match (value1, value2) with
                        | Value.KetId k1, Value.KetId k2 -> q2 |> with_value (KetExpression.Filter(k1, k2))
                        | _ -> $"Invalid KetIds" |> Error

    and eval_qblock ctx (stmts, value) =
        eval_stmts ctx stmts
        ==> fun (ctx) -> eval_quantum ctx value ==> fun value -> value |> Ok

    // syntactic sugar for |set>
    // this is equivalent to create a literal and apply a filter for the elements in the set
    and eval_ket ctx c =
        eval_classic ctx c
        ==> fun (v, graph) ->
                match v with
                | Value.Set set ->
                    let size = set |> width

                    let add_literal (ids, g: QuantumGraph) _ =
                        let k = fresh_ketid ()
                        ids @ [ k ], g.Add(k, KetExpression.Literal 3)

                    let (ids, graph) = seq { 0..size } |> Seq.fold add_literal ([], graph)
                    let literal = fresh_ketid()
                    let graph = graph.Add(literal, KetExpression.Join ids)

                    let map = fresh_ketid ()
                    let graph = graph.Add(map, KetExpression.Map(literal, KetMapOperator.In v))

                    let filter = fresh_ketid ()
                    let graph = graph.Add(filter, KetExpression.Filter(literal, map))
                    (KetId literal, graph) |> Ok
                | _ -> $"Invaid literal constructor. Only sets supported, got {v}" |> Error


    and eval_classic ctx c : Result<Value * QuantumGraph, string> =
        match c with
        | C.Var id -> eval_var ctx id

        | C.BoolLiteral b -> eval_bool ctx b
        | C.IntLiteral i -> eval_int ctx i
        | C.Tuple values -> eval_tuple ctx values
        | C.Set values -> eval_set ctx values
        | C.Range(start, stop) -> eval_range ctx (start, stop)
        | C.Method(args, body) -> eval_method ctx (args, body)

        | C.Add(left, right) -> eval_add ctx (left, right)
        | C.Multiply(left, right) -> eval_multiply ctx (left, right)
        | C.Equals(left, right) -> eval_equals ctx (left, right)
        | C.LessThan(left, right) -> eval_lessthan ctx (left, right)
        | C.And(left, right) -> eval_and ctx (left, right)
        | C.Or(left, right) -> eval_or ctx (left, right)
        | C.Not e -> eval_not ctx e

        | C.Project(value, index) -> eval_project ctx (value, index)
        | C.Index(value, index) -> eval_index ctx (value, index)
        | C.Join(left, right) -> eval_join ctx (left, right)

        | C.If(cond, t, e) -> eval_if ctx (cond, t, e)
        | C.Block(stmts, value) -> eval_block ctx (stmts, value)

        | C.Sample q -> eval_sample ctx q

        | C.CallMethod(method, args) -> eval_callmethod ctx (method, args)

        | C.Element(set) -> eval_element ctx set
        | C.Append(item, set) -> eval_append ctx (item, set)
        | C.Remove(item, set) -> eval_remove ctx (item, set)
        | C.Count(set) -> eval_count ctx set

    and eval_var ctx id =
        match ctx.heap.TryFind id with
        | Some value -> (value, ctx.graph) |> Ok
        | None -> $"Variable not found: {id}" |> Error

    and eval_bool ctx b = (Value.Bool b, ctx.graph) |> Ok

    and eval_int ctx i = (Value.Int i, ctx.graph) |> Ok

    and eval_method ctx (args, body) =
        (Value.Method
            { Args = args
              Body = body
              Context = ctx },
         ctx.graph)
        |> Ok

    and eval_tuple ctx values =
        eval_expression_list ctx values
        ==> fun (values, graph) -> (Tuple values, graph) |> Ok

    and eval_set ctx values =
        eval_expression_list ctx values
        ==> fun (values, graph) -> (Set(Set.ofList values), graph) |> Ok

    and eval_range ctx (start, stop) =
        eval_classic ctx start
        ==> fun (v1, q1) ->
                eval_classic { ctx with graph = q1 } stop
                ==> fun (v2, q2) ->
                        match (v1, v2) with
                        | Value.Int v1, Value.Int v2 ->
                            let values = seq { v1 .. v2 - 1 } |> Seq.map Value.Int
                            (Set(Set.ofSeq values), q2) |> Ok
                        | _ -> $"Range start..stop must be int, got: {start}..{stop}" |> Error

    and eval_add ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) ->
                eval_classic { ctx with graph = q1 } right
                ==> fun (v2, q2) -> (v1 + v2, q2) |> Ok

    and eval_multiply ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) ->
                eval_classic { ctx with graph = q1 } right
                ==> fun (v2, q2) -> (v1 * v2, q2) |> Ok

    and eval_equals ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) ->
                eval_classic { ctx with graph = q1 } right
                ==> fun (v2, q2) -> (v1 == v2, q2) |> Ok

    and eval_lessthan ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) ->
                eval_classic { ctx with graph = q1 } right
                ==> fun (v2, q2) -> (Value.LessThan(v1, v2), q2) |> Ok

    and eval_and ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) ->
                eval_classic { ctx with graph = q1 } right
                ==> fun (v2, q2) -> (Value.And(v1, v2), q2) |> Ok

    and eval_or ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) ->
                eval_classic { ctx with graph = q1 } right
                ==> fun (v2, q2) -> (Value.Or(v1, v2), q2) |> Ok

    and eval_not ctx e =
        eval_classic ctx e ==> fun (v1, graph) -> (Value.Not v1, graph) |> Ok

    and eval_project ctx (value, i) =
        eval_classic ctx value
        ==> fun (value, q) ->
                match value with
                | Value.Tuple t -> (t.[i], q) |> Ok
                | _ -> $"project only avaiable for tuples, got: {value}" |> Error

    and eval_index ctx (value, i) =
        eval_classic ctx value
        ==> fun (value, q1) ->
                eval_classic { ctx with graph = q1 } i
                ==> fun (i, q2) ->
                        match (value, i) with
                        | Value.Tuple t, Value.Int i -> (t.[i], q2) |> Ok
                        | _ -> $"project only avaiable for tuples and int index, got: {value}[{i}]" |> Error

    and eval_join ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) ->
                eval_classic { ctx with graph = q1 } right
                ==> fun (v2, q2) ->
                        match (v1, v2) with
                        | Value.Tuple l, Value.Tuple r -> (Value.Tuple(l @ r), q2) |> Ok
                        | _ -> $"Join only avaiable for tuples, got: {left}, {right}" |> Error

    and eval_if ctx (cond, then_e, else_e) =
        eval_classic ctx cond
        ==> fun (cond, q1) ->
                match cond with
                | Value.Bool true -> eval_classic { ctx with graph = q1 } then_e ==> fun (v2, q2) -> (v2, q2) |> Ok
                | Value.Bool false ->
                    eval_classic { ctx with graph = q1 } else_e
                    ==> fun (v3, graph3) -> (v3, graph3) |> Ok
                | _ -> $"if condition must be a boolean expression, got: {cond}" |> Error

    and eval_block ctx (stmts, value) =
        eval_stmts ctx stmts
        ==> fun (ctx) -> eval_classic ctx value ==> fun value -> value |> Ok

    and eval_sample ctx u =
        let qpu = ctx.qpu

        qpu.Prepare(u, ctx)
        ==> fun (u, graph) ->
                match u with
                | Value.Universe u -> qpu.Measure (u, { ctx with graph = graph })
                | _ -> $"Expecting Prepare to return Universe, got {u}" |> Error

    and eval_callmethod ctx (method, args) =
        setup_method_ctx ctx (method, args)
        ==> fun (body, ctx') -> eval ctx' body ==> fun (v2, q2) -> (v2, q2) |> Ok

    and eval_element ctx set =
        let pick_random (s: Set<Value>) =
            let i = random.Next(s.Count)
            (Set.toList s).[i]

        eval_classic ctx set
        ==> fun (set, q1) ->
                match set with
                | Value.Set s -> (s |> pick_random, q1) |> Ok
                | _ -> $"Append only available for sets, got: {set}" |> Error

    and eval_append ctx (item, set) =
        eval_classic ctx item
        ==> fun (item, q1) ->
                eval_classic { ctx with graph = q1 } set
                ==> fun (set, q2) ->
                        match set with
                        | Value.Set s -> (Value.Set(s.Add item), q2) |> Ok
                        | _ -> $"Append only available for sets, got: {set}" |> Error

    and eval_remove ctx (item, set) =
        eval_classic ctx item
        ==> fun (item, q1) ->
                eval_classic { ctx with graph = q1 } set
                ==> fun (set, q2) ->
                        match set with
                        | Value.Set s -> (Value.Set(s.Remove item), q2) |> Ok
                        | _ -> $"Remove only available for sets, got: {set}" |> Error

    and eval_count ctx set =
        eval_classic ctx set
        ==> fun (v1, q1) ->
                match v1 with
                | Value.Set s -> (Value.Int s.Count, q1) |> Ok
                | _ -> $"Count only available for sets, got: {set}" |> Error

    and eval_expression_list ctx values =
        let rec next items =
            match items with
            | head :: tail, graph ->
                eval_classic { ctx with graph = graph } head
                ==> fun (v1, q1) -> next (tail, q1) ==> fun (v2, q2) -> (v1 :: v2, q2) |> Ok
            | [], graph -> ([], graph) |> Ok

        next (values, ctx.graph)

    and eval_stmts ctx stmts =
        let eval_one (ctx': Result<EvalContext, string>) stmt =
            ctx'
            ==> fun (ctx') ->
                    match stmt with
                    | Let(id, e) ->
                        eval ctx' e
                        ==> fun (value, graph) ->
                                { ctx' with
                                    heap = ctx'.heap.Add(id, value)
                                    graph = graph }
                                |> Ok
                    | Print(msg, expressions) ->
                        printf "%s" msg

                        let print_one ctx' e =
                            ctx'
                            ==> fun (ctx') ->
                                    eval ctx' e
                                    ==> fun (value, graph) ->
                                            printfn "%A" value
                                            { ctx' with graph = graph } |> Ok

                        expressions |> List.fold print_one (ctx' |> Ok)

        stmts |> List.fold eval_one (ctx |> Ok)

    and setup_ctx_with_args ctx ids args =
        let rec next items =
            match items with
            | head :: tail, graph ->
                eval { ctx with graph = graph } head
                ==> fun (v1, q1) -> next (tail, q1) ==> fun (v2, q2) -> (v1 :: v2, q2) |> Ok
            | [], graph -> ([], graph) |> Ok

        next (args, ctx.graph)
        ==> fun (values, graph) ->
                let heap' =
                    values
                    |> List.zip ids
                    |> List.fold (fun (h: Map<Id, Value>) (id, value) -> h.Add(id, value)) ctx.heap

                (heap', graph) |> Ok

    and setup_method_ctx ctx (method, args) =
        eval_classic ctx method
        ==> fun (v1, q1) ->
                match v1 with
                | Value.Method({ Args = ids
                                 Body = body
                                 Context = context }) ->
                    setup_ctx_with_args { ctx with graph = q1 } ids args
                    ==> fun (heap', graph') ->
                            let ctx =
                                { context with
                                    heap = heap'
                                    graph = graph' }

                            (body, ctx) |> Ok
                | _ -> $"Expecting method, got {method}" |> Error


    and eval ctx e : Result<Value * QuantumGraph, string> =
        match e with
        | E.Classic(c, _) -> eval_classic ctx c
        | E.Quantum(q, _) -> eval_quantum ctx q
        | E.Universe (u, _) -> ctx.qpu.Prepare(u, ctx)

    let apply (program: Expression, qpu: QPU) =
        aleph.parser.TypeChecker.start (program)
        ==> fun (e, _) ->
                let ctx =
                    { heap = Map.empty
                      graph = QuantumGraph.empty
                      qpu = qpu }

                eval ctx e
