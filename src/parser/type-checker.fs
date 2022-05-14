namespace aleph.parser

open aleph.parser.ast
open aleph.parser.typed

module TypeChecker =

    let (==>) (input: Result<'a,'b>) ok  =
        Result.bind ok input

    let is_bool msg e = 
        match e with 
        | Classic (_, Type.Bool) -> e |> Ok
        | Classic (e, t) -> msg + $"Expected bool expression, got: ({e}:{t})" |> Error
        | Quantum (e, t) -> msg + $"Expected bool expression, got: ({e}:{t})" |> Error

    let is_set_element msg e =
        match e with 
        | Classic (_, Type.Int _)
        | Classic (_, Type.Bool _)
        | Classic (_, Type.Tuple _) -> e |> Ok
        | Classic (e, t) -> msg + $"Expected int, bool or tuple expression, got: ({e}:{t})" |> Error
        | Quantum (e, t) -> msg + $"Expected int, bool or tuple expression, got: ({e}:{t})" |> Error

    let is_tuple msg e =
        match e with 
        | Classic (_, Type.Tuple _) -> e |> Ok
        | Classic (e, t) -> msg + $"Expected tuple expression, got: ({e}:{t})" |> Error
        | Quantum (e, t) -> msg + $"Expected tuple expression, got: ({e}:{t})" |> Error

    let is_bool_or_int msg e = 
        match e with 
        | Classic (_, Type.Bool)
        | Classic (_, Type.Int) -> e |> Ok
        | Classic (e, t) -> msg + $"Expected bool or int expression, got: ({e}:{t})" |> Error
        | Quantum (e, t) -> msg + $"Expected bool or int expression, got: ({e}:{t})" |> Error

    // For a list of typed Expressions (E), returns two list: one with just the expression
    // elements and a second with just the Types.
    // All must be Classic expressions.
    let unzip_classic values =
        let rec next items =
            match items with
            | head :: tail ->
                match head with
                | Classic (e,t) ->
                    next tail
                    ==> fun (expressions, types) ->
                        (e :: expressions, t::types) |> Ok
                | _ ->
                    $"Invalid element: {head}. Expecting a classic type." |> Error
            | [] -> ([], []) |> Ok
        next values

    type AnyType =
        | Type of Type
        | QType of QType

    type TypeContext = Map<Id, AnyType>

    let rec typecheck (e: Expression, ctx: TypeContext) =
        match e with 
        | Expression.Var id -> typecheck_var (id, ctx)
        | Expression.Bool b -> typecheck_bool (b, ctx)
        | Expression.Int i  -> typecheck_int (i, ctx)
        | Expression.Tuple values -> typecheck_tuple (values, ctx)
        | Expression.Set values -> typecheck_set (values, ctx)

        | Expression.Not value -> typecheck_not (value, ctx)
        | Expression.Or values -> typecheck_or (values, ctx)
        | Expression.And values -> typecheck_and (values, ctx)

        | Expression.Range _

        | Expression.Method _
        | Expression.CallMethod _

        | Expression.Equals _
        | Expression.LessThan _

        | Expression.Add _
        | Expression.Multiply _
        | Expression.Join _

        | Expression.Project _
        | Expression.Block _
        | Expression.If _
        | Expression.Summarize _

        | Expression.Ket _
        | Expression.AllKet _

        | Expression.QMethod _
        | Expression.CallQMethod _

        | Expression.Sample _
        | Expression.Measure _
        | Expression.Solve _ ->
            $"Expression {e} has not been implemented yet!" |> Error

    and typecheck_var (id, ctx) =
        match ctx.TryFind id with
        | Some t -> 
            match t with
            | Type t -> (Classic (C.Var id, t), ctx) |> Ok
            | QType t -> (Quantum (Q.Var id, t), ctx) |> Ok
        | None -> $"Unknown variable: {id}" |> Error

    and typecheck_bool (b, ctx) =
        (Classic (BoolLiteral b, Type.Bool), ctx) |> Ok

    and typecheck_int (i, ctx) =
        (Classic (IntLiteral i, Type.Int), ctx) |> Ok

    and typecheck_tuple (values, ctx) =
        typecheck_expression_list (is_bool_or_int  "Invalid tuple element. ") (values, ctx)
        ==> fun (values, ctx) ->
            unzip_classic values
            ==> fun (exps, types) ->
                (Classic (C.Tuple exps, Type.Tuple types), ctx) |> Ok

    and typecheck_set (values, ctx) =
        match values with
        | [] -> (Classic (C.Set [], Type.Set (Type.Tuple [])), ctx) |> Ok
        | _ ->
            typecheck_expression_list (is_set_element "Invalid set element. ") (values, ctx)
            ==> fun (values, ctx) ->
                unzip_classic values
                ==> fun (exps, types) ->
                    let head = types.Head
                    let allEqual = types.Tail |> List.forall (fun t -> t = head)
                    if allEqual then
                        (Classic (C.Set exps, Type.Set head), ctx) |> Ok
                    else
                        "All tuples in a set must be of the same type." |> Error

    and typecheck_and (values, ctx) =
        typecheck_expression_list (is_bool "Invalid And element. ") (values, ctx)
        ==> fun (values, ctx) ->
            unzip_classic values
            ==> fun (exps, _) ->
                (Classic (C.And exps, Type.Bool), ctx) |> Ok

    and typecheck_or (values, ctx) =
        typecheck_expression_list (is_bool "Invalid Or element. ") (values, ctx)
        ==> fun (values, ctx) ->
            unzip_classic values
            ==> fun (exps, _) ->
                (Classic (C.Or exps, Type.Bool), ctx) |> Ok

    and typecheck_not (value, ctx) =
        typecheck(value, ctx)
        ==> fun (value, ctx) ->
            match value with 
            | Classic (v, Type.Bool) ->
                (Classic (C.Not v, Type.Bool), ctx) |> Ok
            | _ ->
                $"Not expressions require boolean arguments, got: {value}" |> Error

    // Typechecks an untyped expression list. Receives a callback such that
    // each expression can be validated. If all the expressions are valid
    // it returns the list of corresponding typed expressions.
    and typecheck_expression_list is_valid_expression (values, ctx) =
        let rec next (items, ctx) =
            match items with
            | head :: tail ->
                typecheck (head, ctx)
                ==> fun (head, ctx) ->
                    is_valid_expression head
                    ==> fun head ->
                        next (tail, ctx)
                        ==> fun (tail, ctx) ->
                            (head :: tail, ctx) |> Ok
            | [] -> ([], ctx) |> Ok
        next (values, ctx)


