namespace aleph.runtime

open aleph.parser.ast
open aleph.parser.ast.typed
open aleph.parser.TypeChecker

module EvalV5 =
    open System
    let random = System.Random()

    let (==>) (input: Result<'a, 'b>) ok = Result.bind ok input

    let mutable max_ket = 0


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

    and KetId =
        | One of int
        | Many of int list

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

    and QuantumGraph(q:Map<Value, KetExpression>) =
        member self.map = q

        member self.append(k: Value, v: KetExpression) : QuantumGraph =
            QuantumGraph (q.Add (k, v))

        static member empty: QuantumGraph =
            QuantumGraph Map.empty

        static member (+)(l: QuantumGraph, r: QuantumGraph) =
            QuantumGraph (Map.foldBack Map.add (l.map) (r.map))

    and KetOperator =
        | Constant of c: Value
        | Add
        | Multiply
        | If
        | Equals
        | LessThan
        | Not
        | And
        | Or
        | In of c: Value

    and KetExpression =
        | Literal of size: int
        | Join of values: Value * Value
        | Project of source: Value * index: int
        | Map of input: Value * lambda: KetOperator
        | Filter of input: Value * filter: Value

    and EvalContext =
        { heap: Map<Id, Value * QuantumGraph>
          qpu: QPU
          callerCtx: EvalContext option }

    and QPU =
        abstract Prepare: U * EvalContext -> Result<Value, string>
        abstract Measure: IUniverse -> Result<Value, string>

    // let join (q1: QuantumGraph) (q2: QuantumGraph) = Map.foldBack Map.add q2 q1

    // let append (key: KetId, value: KetExpression) (q:QuantumGraph): QuantumGraph =
        

    let fresh_ketid () =
        max_ket <- max_ket + 1
        KetId (One max_ket)


    let rec eval_quantum ctx q : Result<Value * QuantumGraph, string> =
        match q with
        | Q.Var id -> eval_var ctx id

        | Q.Constant value ->
            eval_classic ctx value
            ==> fun (v, q:QuantumGraph) ->
                let k = fresh_ketid()
                let exp =  KetExpression.Map (k, KetOperator.Constant v)
                (k, q.append(k, exp)) |> Ok

        // | Q.Ket c ->
            // eval_classic ctx c
            // ==> fun (set, q:QuantumGraph) ->
            //     let lit = fresh_ketid()
            //     let q = q.append(lit, KetExpression.Literal 3)
            //     let mark = fresh_ketid()
            //     let q = q.append(mark, KetExpression.Map (lit, KetOperator.In set))
            //     let filter = fresh_ketid()
            //     let q = q.append(filter, KetExpression.Filter(lit, mark))
            //     (lit, q) |> Ok


        | Q.KetAll size ->
            eval_classic ctx size
            ==> fun (value, graph:QuantumGraph) ->
                match value with
                | Value.Int n -> 
                    let k = fresh_ketid()
                    (k, graph.append(k, KetExpression.Literal n)) |> Ok
                | _ -> $"Invalid KetAll size: {value}" |> Error

        | Q.Equals (left, right) -> 
            eval_quantum ctx left
            ==> fun (v1, q1) ->
                eval_quantum ctx right
                ==> fun (v2, q2) ->
                match (v1, v2) with
                | Value.KetId (One k1), Value.KetId (One k2) -> 
                    let k = fresh_ketid()
                    let exp =  KetExpression.Map (KetId (KetId.Many [k1; k2]), KetOperator.Equals)
                    (k, (q1 + q2).append(k, exp)) |> Ok
                | _ -> $"Invalid KetIds" |> Error

        // | Add (left, right) -> prepare_add (left, right, ctx)
        // | Multiply (left, right) -> prepare_multiply (left, right, ctx)

        // | Not q -> prepare_not (q, ctx)
        // | And (left, right) -> prepare_and (left, right, ctx)
        // | Or (left, right) -> prepare_or (left, right, ctx)

        // | Project (q, index) -> prepare_project (q, index, ctx)
        // | Index (q, index) -> prepare_index (q, index, ctx)
        // | Join (left, right) -> prepare_join (left, right, ctx)

        // | IfQuantum (condition, then_q, else_q) -> prepare_if_q (condition, then_q, else_q, ctx)
        // | IfClassic (condition, then_q, else_q) -> prepare_if_c (condition, then_q, else_q, ctx)
        // | Filter (ket, condition, hint) -> prepare_filter (ket, condition, hint, ctx)

        // | Q.Block (stmts, value) -> prepare_block (stmts, value, ctx)
        
        // | Q.CallMethod (method, args) -> prepare_callmethod (method, args, ctx)
        | _ -> $"Not implemented q: {q}" |> Error

    and eval_classic ctx c =
        match c with
        | C.Var id -> eval_var ctx id

        | C.BoolLiteral b -> eval_bool b
        | C.IntLiteral i -> eval_int i
        | C.Tuple values -> eval_tuple ctx values
        | C.Set values -> eval_set ctx values
        | C.Range (start, stop) -> eval_range ctx (start, stop)
        | C.Method (args, body) -> eval_method ctx (args, body)

        | C.Add (left, right) -> eval_add ctx (left, right)
        | C.Multiply (left, right) -> eval_multiply ctx (left, right)
        | C.Equals (left, right) -> eval_equals ctx (left, right)
        | C.LessThan (left, right) -> eval_lessthan ctx (left, right)
        | C.And (left, right) -> eval_and ctx (left, right)
        | C.Or (left, right) -> eval_or ctx (left, right)
        | C.Not e -> eval_not ctx e

        | C.Project (value, index) -> eval_project ctx (value, index)
        | C.Index (value, index) -> eval_index ctx (value, index)
        | C.Join (left, right) -> eval_join ctx (left, right)

        | C.If (cond, t, e) -> eval_if ctx (cond, t, e)
        | C.Block (stmts, value) -> eval_block ctx (stmts, value)

        | C.Sample q -> eval_sample ctx q

        | C.CallMethod (method, args) -> eval_callmethod ctx (method, args)

        | C.Element (set) -> eval_element ctx set
        | C.Append (item, set) -> eval_append ctx (item, set)
        | C.Remove (item, set) -> eval_remove ctx (item, set)
        | C.Count (set) -> eval_count ctx set

    and eval_var ctx id =
        match ctx.heap.TryFind id with
        | Some value -> value |> Ok
        | _ ->
            match ctx.callerCtx with
            | Some ctx' -> eval_var ctx' id
            | None -> $"Variable not found: {id}" |> Error

    and eval_bool b = (Value.Bool b, QuantumGraph.empty) |> Ok

    and eval_int i = (Value.Int i, QuantumGraph.empty) |> Ok

    and eval_method ctx (args, body) =
        (Value.Method
            { Args = args
              Body = body
              Context = ctx },
         QuantumGraph.empty)
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
                eval_classic ctx stop
                ==> fun (v2, q2) ->
                        match (v1, v2) with
                        | Value.Int v1, Value.Int v2 ->
                            let values = seq { v1 .. v2 - 1 } |> Seq.map Value.Int
                            (Set(Set.ofSeq values), q1 + q2) |> Ok
                        | _ -> $"Range start..stop must be int, got: {start}..{stop}" |> Error

    and eval_add ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) -> eval_classic ctx right ==> fun (v2, q2) -> (v1 + v2, q1 + q2) |> Ok

    and eval_multiply ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) -> eval_classic ctx right ==> fun (v2, q2) -> (v1 * v2, q1 + q2) |> Ok

    and eval_equals ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) -> eval_classic ctx right ==> fun (v2, q2) -> (v1 == v2, q1 + q2) |> Ok

    and eval_lessthan ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) ->
                eval_classic ctx right
                ==> fun (v2, q2) -> (Value.LessThan(v1, v2), q1 + q2) |> Ok

    and eval_and ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) -> eval_classic ctx right ==> fun (v2, q2) -> (Value.And(v1, v2), q1 + q2) |> Ok

    and eval_or ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) -> eval_classic ctx right ==> fun (v2, q2) -> (Value.Or(v1, v2), q1 + q2) |> Ok

    and eval_not ctx e =
        eval_classic ctx e ==> fun (v1, q1) -> (Value.Not v1, q1) |> Ok

    and eval_project ctx (value, i) =
        eval_classic ctx value
        ==> fun (value, q) ->
                match value with
                | Value.Tuple t -> (t.[i], q) |> Ok
                | _ -> $"project only avaiable for tuples, got: {value}" |> Error

    and eval_index ctx (value, i) =
        eval_classic ctx value
        ==> fun (value, q1) ->
                eval_classic ctx i
                ==> fun (i, q2) ->
                        match (value, i) with
                        | Value.Tuple t, Value.Int i -> (t.[i], q1 + q2) |> Ok
                        | _ -> $"project only avaiable for tuples and int index, got: {value}[{i}]" |> Error

    and eval_join ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) ->
                eval_classic ctx right
                ==> fun (v2, q2) ->
                        match (v1, v2) with
                        | Value.Tuple l, Value.Tuple r -> (Value.Tuple(l @ r), q1 + q2) |> Ok
                        | _ -> $"Join only avaiable for tuples, got: {left}, {right}" |> Error

    and eval_if ctx (cond, then_e, else_e) =
        eval_classic ctx cond
        ==> fun (cond, q1) ->
                match cond with
                | Value.Bool true -> eval_classic ctx then_e ==> fun (v2, q2) -> (v2, q1 + q2) |> Ok
                | Value.Bool false -> eval_classic ctx else_e ==> fun (v3, q3) -> (v3, q1 + q3) |> Ok
                | _ -> $"if condition must be a boolean expression, got: {cond}" |> Error

    and eval_block ctx (stmts, value) =
        eval_stmts ctx stmts
        ==> fun (ctx) -> eval_classic ctx value ==> fun value -> value |> Ok

    and eval_sample ctx u =
        let qpu = ctx.qpu

        qpu.Prepare(u, ctx)
        ==> fun u ->
                match u with
                | Value.Universe u -> qpu.Measure u ==> fun (v) -> (v, QuantumGraph.empty) |> Ok
                | _ -> $"Expecting Prepare to return Universe, got {u}" |> Error

    and eval_callmethod ctx (method, args) =
        setup_method_body ctx (method, args)
        ==> fun (body, q1, ctx') -> eval ctx' body ==> fun (v2, q2) -> (v2, q1 + q2) |> Ok

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
                eval_classic ctx set
                ==> fun (set, q2) ->
                        match set with
                        | Value.Set s -> (Value.Set(s.Add item), q1 + q2) |> Ok
                        | _ -> $"Append only available for sets, got: {set}" |> Error

    and eval_remove ctx (item, set) =
        eval_classic ctx item
        ==> fun (item, q1) ->
                eval_classic ctx set
                ==> fun (set, q2) ->
                        match set with
                        | Value.Set s -> (Value.Set(s.Remove item), q1 + q2) |> Ok
                        | _ -> $"Remove only available for sets, got: {set}" |> Error

    and eval_count ctx set =
        eval_classic ctx set
        ==> fun (set, q1) ->
                match set with
                | Value.Set s -> (Value.Int s.Count, q1) |> Ok
                | _ -> $"Count only available for sets, got: {set}" |> Error

    and eval_expression_list ctx values =
        let rec next items =
            match items with
            | head :: tail ->
                eval_classic ctx head
                ==> fun (head, q1) -> next tail ==> fun (tail, q2) -> (head :: tail, q1 + q2) |> Ok
            | [] -> ([], QuantumGraph.empty) |> Ok

        next values

    and eval_stmts ctx stmts =
        let eval_one (ctx': Result<EvalContext, string>) stmt =
            ctx'
            ==> fun (ctx') ->
                    match stmt with
                    | Let (id, e) -> eval ctx' e ==> fun value -> { ctx' with heap = ctx'.heap.Add(id, value) } |> Ok
                    | Print (msg, expressions) ->
                        printf "%s" msg

                        let print_one ctx' e =
                            ctx'
                            ==> fun (ctx') ->
                                    eval ctx' e
                                    ==> fun (value, _) ->
                                            printfn "%A" value
                                            ctx' |> Ok

                        expressions |> List.fold print_one (ctx' |> Ok)

        let ctx =
            { ctx with
                heap = Map.empty
                callerCtx = ctx |> Some }

        stmts |> List.fold eval_one (ctx |> Ok)

    and setup_args ctx ids args =
        let add_argument (heap': Result<Map<Id, Value * QuantumGraph>, string>) (id, value) =
            heap'
            ==> fun heap' -> eval ctx value ==> fun (value) -> heap'.Add(id, value) |> Ok

        args |> List.zip ids |> List.fold add_argument (Map.empty |> Ok)

    and setup_method_body ctx (method, args) =
        eval_classic ctx method
        ==> fun (v1, q1) ->
                match v1 with
                | Value.Method ({ Args = ids
                                  Body = body
                                  Context = context }) ->
                    setup_args ctx ids args
                    ==> fun args_map ->
                            let args_map =
                                // If the method comes from a variable, add it to the context
                                // so it can be invoked recursively:
                                match method with
                                | C.Var id -> args_map.Add(id, (v1, QuantumGraph.empty))
                                | _ -> args_map

                            let ctx =
                                { ctx with
                                    heap = args_map
                                    callerCtx = context |> Some }

                            (body, q1, ctx) |> Ok
                | _ -> $"Expecting method, got {method}" |> Error


    and eval ctx e : Result<Value * QuantumGraph, string> =
        match e with
        | E.Classic (c, _) -> eval_classic ctx c
        | E.Quantum (q, _) -> eval_quantum ctx q
        | _ -> "Not implemented" |> Error

    // | E.Universe (u, _) -> ctx.qpu.Prepare(u, ctx)


    let apply (program: Expression, qpu: QPU) =
        aleph.parser.TypeChecker.start (program)
        ==> fun (e, _) ->
                let ctx =
                    { heap = Map.empty
                      qpu = qpu
                      callerCtx = None }

                eval ctx e
