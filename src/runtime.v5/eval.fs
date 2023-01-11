namespace aleph.runtime

open aleph.parser.ast
open aleph.parser.ast.typed
open aleph.parser.TypeChecker

module EvalV5 =
    open System
    let random = System.Random()

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

    and KetId = 
        | Id of int
        | Tuple of KetId * KetId

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

    and QuantumGraph = Map<KetId, KetExpression>

    and KetOperator =
        | Add
        | Multiply
        | If
        | Equals
        | LessThan
        | Not
        | And
        | Or

    and KetExpression = 
        | Literal of size: int
        | Join of values: KetId * KetId
        | Project of source: KetId * index: int
        | Map of input: KetId * lambda: KetOperator
        | Filter of input: KetId * filter: KetId

    and EvalContext =
        { heap: Map<Id, Value * QuantumGraph>
          qpu: QPU
          callerCtx: EvalContext option }

    and QPU =
        abstract Prepare: U * EvalContext -> Result<Value, string>
        abstract Measure: IUniverse -> Result<Value, string>

    let mutable max_ket = 0

    let join (q1:QuantumGraph) (q2:QuantumGraph) = 
        Map.foldBack Map.add q2 q1

    let rec eval ctx e  : Result<Value * QuantumGraph, string> =
        match e with
        | E.Classic (c, _) -> eval_classic ctx c
        | _ -> "Not implemented" |> Error
        
        // | E.Quantum (q, _) -> eval_quantum (q, ctx)
        // | E.Universe (u, _) -> ctx.qpu.Prepare(u, ctx)

    // and eval_quantum (q, ctx) =
    //     match q with
    //     | Q.Var id -> eval_var (id, ctx)
    //     | _ ->
    //         max_ket <- max_ket + 1

    //         (Value.Ket
    //             { Id = max_ket
    //               StatePrep = q
    //               Context = ctx },
    //          ctx)
    //         |> Ok

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

    and eval_bool b = (Value.Bool b, Map.empty) |> Ok

    and eval_int i = (Value.Int i, Map.empty) |> Ok

    and eval_method ctx (args, body) =
        (Value.Method
            { Args = args
              Body = body
              Context = ctx },
            Map.empty)
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
                            (Set(Set.ofSeq values), join q1 q2) |> Ok
                        | _ -> $"Range start..stop must be int, got: {start}..{stop}" |> Error

    and eval_add ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) -> eval_classic ctx right ==> fun (v2, q2) -> (v1 + v2, join q1 q2) |> Ok

    and eval_multiply ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) -> eval_classic ctx right ==> fun (v2, q2) -> (v1 * v2, join q1 q2) |> Ok

    and eval_equals ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) -> eval_classic ctx right ==> fun (v2, q2) -> (v1 == v2, join q1 q2) |> Ok

    and eval_lessthan ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) -> eval_classic ctx right ==> fun (v2, q2) -> (Value.LessThan(v1, v2), join q1 q2) |> Ok

    and eval_and ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) -> eval_classic ctx right ==> fun (v2, q2) -> (Value.And(v1, v2), join q1 q2) |> Ok

    and eval_or ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) -> eval_classic ctx right ==> fun (v2, q2) -> (Value.Or(v1, v2), join q1 q2) |> Ok

    and eval_not ctx e=
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
                        | Value.Tuple t, Value.Int i -> (t.[i], join q1 q2) |> Ok
                        | _ -> $"project only avaiable for tuples and int index, got: {value}[{i}]" |> Error

    and eval_join ctx (left, right) =
        eval_classic ctx left
        ==> fun (v1, q1) ->
                eval_classic ctx right
                ==> fun (v2, q2) ->
                        match (v1, v2) with
                        | Value.Tuple l, Value.Tuple r -> (Value.Tuple(l @ r), join q1 q2) |> Ok
                        | _ -> $"Join only avaiable for tuples, got: {left}, {right}" |> Error

    and eval_if ctx (cond, then_e, else_e) =
        eval_classic ctx cond
        ==> fun (cond, q1) ->
                match cond with
                | Value.Bool true -> eval_classic ctx then_e ==> fun (v2, q2) -> (v2, join q1 q2) |> Ok
                | Value.Bool false -> eval_classic ctx else_e ==> fun (v3, q3) -> (v3, join q1 q3) |> Ok
                | _ -> $"if condition must be a boolean expression, got: {cond}" |> Error

    and eval_block ctx (stmts, value) =
        eval_stmts  ctx stmts
        ==> fun (ctx) ->
                eval_classic ctx value
                ==> fun value -> value |> Ok

    and eval_sample ctx u =
        let qpu = ctx.qpu

        qpu.Prepare(u, ctx)
        ==> fun u ->
                match u with
                | Value.Universe u -> qpu.Measure u ==> fun (v) -> (v, Map.empty) |> Ok
                | _ -> $"Expecting Prepare to return Universe, got {u}" |> Error

    and eval_callmethod ctx (method, args) =
        setup_method_body ctx (method, args)
        ==> fun (body, q1, ctx') ->
                eval ctx' body
                ==> fun (v2, q2) ->
                        // return the heap back to the original state
                        let ctx = { ctx' with heap = ctx.heap }
                        (v2, join q1 q2) |> Ok

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
                        | Value.Set s -> (Value.Set(s.Add item), join q1 q2) |> Ok
                        | _ -> $"Append only available for sets, got: {set}" |> Error

    and eval_remove ctx (item, set) =
        eval_classic ctx item
        ==> fun (item, q1) ->
                eval_classic ctx set
                ==> fun (set, q2) ->
                        match set with
                        | Value.Set s -> (Value.Set(s.Remove item), join q1 q2) |> Ok
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
                ==> fun (head, q1) -> next tail ==> fun (tail, q2) -> (head :: tail, join q1 q2) |> Ok
            | [] -> ([], Map.empty) |> Ok

        next values

    and eval_stmts ctx stmts : Result<EvalContext, string> =
        let eval_one ctx' stmt =
            ctx'
            ==> fun (ctx') ->
                    match stmt with
                    | Let (id, e) ->
                        eval ctx' e
                        ==> fun (value) -> { ctx' with heap = ctx'.heap.Add(id, value) } |> Ok
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
                                | C.Var id -> args_map.Add(id, (v1, Map.empty))
                                | _ -> args_map

                            let ctx =
                                { ctx with
                                    heap = args_map
                                    callerCtx = context |> Some }

                            (body, q1, ctx) |> Ok
                | _ -> $"Expecting method, got {method}" |> Error

    let apply (program: Expression, qpu: QPU) =
        aleph.parser.TypeChecker.start (program)
        ==> fun (e, _) ->
                let ctx =
                    { heap = Map.empty
                      qpu = qpu
                      callerCtx = None }
                eval ctx e
