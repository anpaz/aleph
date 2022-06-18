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

    type QPU =
        abstract Allocate: Q -> Ket
        abstract Reset: Unit -> Unit
        abstract Prepare: Ket * ValueContext -> Result<Ket * ValueContext, string>
        abstract Measure: Ket -> Value

    and ValueContext = {
        qpu: QPU
        assignments: Map<string, Value>
    }

    let rec eval (program: Expression, ctx) =
        typecheck (program, Map.empty)
        ==> fun (e, _) ->
            match e with
            | Quantum (q, QType.Ket _) ->
                (Value.Ket (ctx.qpu.Allocate q), ctx) |> Ok
            | Classic (c, _) ->
                eval_classic (c, ctx)

    and eval_classic (c, ctx) =
        match c with
        | C.Var id -> eval_var (id, ctx)

        | C.BoolLiteral b -> eval_bool (b, ctx) 
        | C.IntLiteral i -> eval_int (i, ctx)
        | C.Tuple values -> eval_tuple (values, ctx)
        | C.Set values -> eval_set (values, ctx)

        | C.Sample _
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
        | C.Block _
        | C.If _
        | C.Summarize _ ->
            "Not implemented" |> Error

    and eval_var (id, ctx) =
        match ctx.assignments.TryFind id with
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
