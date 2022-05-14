namespace aleph.parser

open aleph.parser.ast
open aleph.parser.typed

module TypeChecker =

    let (==>) (input: Result<'a,'b>) ok  =
        Result.bind ok input

    let is_bool = function
    | Classic (_, Type.Bool) -> true
    | _ -> false

    let is_tuple = function
    | Classic (_, Type.Tuple _) -> true
    | _ -> false

    let is_bool_or_int = function
    | Classic (_, Type.Bool)
    | Classic (_, Type.Int) -> true
    | _ -> false

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
                    $"Invalid element: {head}" |> Error
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

        | Expression.And values -> typecheck_and (values, ctx)

        | Expression.Range _

        | Expression.Method _
        | Expression.CallMethod _

        | Expression.Not _
        | Expression.Or _
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
        typecheck_expression_list is_bool_or_int (values, ctx)
        ==> fun (values, ctx) ->
            unzip_classic values
            ==> fun (exps, types) ->
                (Classic (C.Tuple exps, Type.Tuple types), ctx) |> Ok

    and typecheck_set (values, ctx) =
        match values with
        | [] -> (Classic (C.Set [], Type.Set (Type.Tuple [])), ctx) |> Ok
        | _ ->
            typecheck_expression_list is_tuple (values, ctx)
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
        match values with
        | [] -> "And expressions require at least one element" |> Error
        | _ ->
            typecheck_expression_list is_bool (values, ctx)
            ==> fun (values, ctx) ->
                unzip_classic values
                ==> fun (exps, _) ->
                    (Classic (C.And exps, Type.Bool), ctx) |> Ok

    and typecheck_or (values, ctx) =
        match values with
        | [] -> "Or expressions require at least one element" |> Error
        | _ ->
            typecheck_expression_list is_bool (values, ctx)
            ==> fun (values, ctx) ->
                unzip_classic values
                ==> fun (exps, _) ->
                    (Classic (C.Or exps, Type.Bool), ctx) |> Ok

    // Typechecks an untyped expression list. Receives a callback such that
    // each expression can be validated. If all the expressions are valid
    // it returns the list of corresponding typed expressions.
    and typecheck_expression_list is_valid_expression (values, ctx) =
        let rec next (items, ctx) =
            match items with
            | head :: tail ->
                typecheck (head, ctx)
                ==> fun (head, ctx) ->
                    if is_valid_expression head then
                        next (tail, ctx)
                        ==> fun (tail, ctx) ->
                            (head :: tail, ctx) |> Ok
                    else
                        $"Invalid element: {head}" |> Error
            | [] -> ([], ctx) |> Ok
        next (values, ctx)


