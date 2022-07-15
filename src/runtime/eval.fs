namespace aleph.runtime

open aleph.parser.ast
open aleph.parser.ast.typed
open aleph.parser.TypeChecker

module Eval =

    let (==>) (input: Result<'a,'b>) ok  =
        Result.bind ok input

    type Ket = {
        Id: int
        StatePrep: Q
    }

    type Value =
        | Bool of bool
        | Int of int
        | Tuple of Value list
        | Set of Set<Value>
        | Method of string list * E
        | Ket of Ket

        static member (+) (l : Value, r: Value) =
            match (l, r) with
            | Value.Int l, Value.Int r -> Value.Int (l + r)
            | _ -> failwith "+ only supported for ints, got {l} + {r}"

        static member (==) (l : Value, r: Value) =
            match (l, r) with
            | Value.Int l, Value.Int r -> Value.Bool (l = r)
            | _ -> failwith "= only supported for ints, got {l} == {r}"

        static member Not (l : Value) =
            match l with
            | Value.Bool b -> Value.Bool (not b)
            | _ -> failwith "not only supported for Bools, got {l}"

        static member And (l : Value, r: Value) =
            match (l, r) with
            | Value.Bool l, Value.Bool r -> Value.Bool (l && r)
            | _ -> failwith "= only supported for ints, got {l} && {r}"

        static member Or (l : Value, r: Value) =
            match (l, r) with
            | Value.Bool l, Value.Bool r -> Value.Bool (l || r)
            | _ -> failwith "= only supported for ints, got {l} || {r}"
    type QPU =
        abstract Assign: Q -> Ket
        abstract Reset: Unit -> Unit
        abstract Prepare: Ket * ValueContext -> Result<Ket * ValueContext, string>
        abstract Measure: Ket -> Value

    and ValueContext = {
        qpu: QPU
        heap: Map<string, Value>
        types: TypeContext
    }

    let rec run (program: Expression, ctx) =
        typecheck (program, ctx.types)
        ==> fun (e, types') ->
            let ctx = { ctx  with types = types' }
            eval (e, ctx)

    and eval (e, ctx) =
        match e with
        | Quantum (q, QType.Ket _) ->
            (Value.Ket (ctx.qpu.Assign q), ctx) |> Ok
        | Classic (c, _) ->
            eval_classic (c, ctx)

    and eval_classic (c, ctx) =
        match c with
        | C.Var id -> eval_var (id, ctx)

        | C.BoolLiteral b -> eval_bool (b, ctx) 
        | C.IntLiteral i -> eval_int (i, ctx)
        | C.Tuple values -> eval_tuple (values, ctx)
        | C.Set values -> eval_set (values, ctx)

        | C.Block (stmts, value) -> eval_block (stmts, value, ctx)

        | C.Sample q -> eval_sample (q, ctx)

        | C.Range _
        | C.Not _
        | C.And _
        | C.Or _
        | C.Equals _
        | C.LessThan _
        | C.Add _
        | C.Multiply _
        | C.Method _
        | C.CallMethod _
        | C.Join _
        | C.Project _
        | C.Index _
        | C.If _
        | C.Summarize _ ->
            "Not implemented" |> Error

    and eval_var (id, ctx) =
        match ctx.heap.TryFind id with
        | Some value ->
            (value, ctx) |> Ok
        | _ ->
            $"Variable not found: {id}" |> Error

    and eval_bool (b, ctx) =
        (Value.Bool b, ctx) |> Ok

    and eval_int (i, ctx) =
        (Value.Int i, ctx) |> Ok

    and eval_tuple (values, ctx) =
        let literal_value = function
            | C.BoolLiteral b, ctx -> (Value.Bool b, ctx) |> Ok
            | C.IntLiteral i, ctx -> (Value.Int i, ctx) |> Ok
            | _ -> "Invalid tuple value." |> Error
        eval_expression_list literal_value (values, ctx)
        ==> fun (values, ctx) -> (Tuple values, ctx) |> Ok

    and eval_set (values, ctx) =
        let literal_value = function
            | C.BoolLiteral b, ctx -> (Value.Bool b, ctx) |> Ok
            | C.IntLiteral i,ctx -> (Value.Int i, ctx) |> Ok
            | _ -> "Invalid tuple value." |> Error
        let single_value = function
            | C.BoolLiteral b, ctx -> (Value.Bool b, ctx) |> Ok
            | C.IntLiteral i, ctx -> (Value.Int i, ctx) |> Ok
            | C.Tuple t, ctx ->
                eval_expression_list literal_value (t, ctx)
                ==> fun (t, ctx) -> (Tuple t, ctx) |> Ok
            | _ -> "Invalid set value." |> Error
        eval_expression_list single_value (values, ctx)
        ==> fun (values, ctx) -> (Set (Set.ofList values), ctx) |> Ok

    and eval_block (stmts,value, ctx) =
        eval_stmts (stmts, ctx) 
        ==> fun (ctx) -> eval_classic (value, ctx)
        
    and eval_sample (q, ctx) =
        let qpu = ctx.qpu
        let ket = qpu.Assign q
        qpu.Reset ()
        qpu.Prepare (ket, ctx)
        ==> fun (_) ->
            (qpu.Measure ket, ctx) |> Ok

    and eval_expression_list check_item_value (values, ctx) =
        let rec next (items, ctx: ValueContext) =
            match items with
            | head :: tail ->
                check_item_value (head, ctx)
                ==> fun (head, ctx) ->
                    next (tail, ctx)
                    ==> fun (tail, ctx) ->
                        (head :: tail, ctx) |> Ok
            | [] -> ([], ctx) |> Ok
        next (values, ctx)

    and eval_stmts (stmts, ctx) =
        let eval_one ctx' stmt =
            ctx'
            ==> fun(ctx') -> 
                match stmt with 
                | Let (id, e) ->
                    eval (e, ctx')
                    ==> fun (value, ctx') ->
                        { ctx' with heap = ctx'.heap.Add  (id, value) } |> Ok
                | Print (msg, expressions) ->
                    printfn "%s" msg
                    let print_one ctx' e =
                        ctx' ==> fun (ctx') -> 
                        eval (e, ctx') 
                        ==> fun (value, ctx') ->
                            printfn "%A" value
                            ctx' |> Ok
                    expressions
                    |> List.fold print_one (ctx' |> Ok)
        stmts
        |> List.fold eval_one (ctx |> Ok)
