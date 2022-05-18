namespace aleph.parser

open aleph.parser.ast
open aleph.parser.ast.typed

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

    // Gets a typed expression (E), and returns
    // its quantum equivalent. If the expression is already quantum returns
    // itself, otherwise return the corresponding elements for a quantum Literal.
    let make_q e =
        match e with 
        | Quantum (l, t) -> (l, t) |> Ok
        | Classic (l, Type.Set (Type.Tuple t)) -> (Literal l, QType.Ket t) |> Ok
        | Classic (l, Type.Set t) -> (Literal l, QType.Ket [t]) |> Ok
        | Classic (l, Type.Int) -> (Literal (C.Set [l]), QType.Ket [Type.Int]) |> Ok
        | Classic (l, Type.Bool) -> (Literal (C.Set [l]), QType.Ket [Type.Bool]) |> Ok
        | Classic (l, Type.Tuple t) -> (Literal (C.Set [l]), QType.Ket t) |> Ok
        | _ -> "Cannot create quantum literal from {e}" |> Error 


    type TypeContext = Map<Id, AnyType>

    let QInt = QType.Ket [Type.Int]
    let QBool = QType.Ket [Type.Bool]

    let add_to_context (ctx:TypeContext) (arguments: (Id * AnyType) list) : TypeContext =
        arguments 
        |> List.fold (fun state arg -> state.Add arg) ctx

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

        | Expression.Add (left, right) -> typecheck_add (left, right, ctx)
        | Expression.Multiply (left, right) -> typecheck_multiply (left, right, ctx)

        | Expression.Range (start, stop) -> typecheck_range (start, stop, ctx)

        | Expression.Method (arguments, body) -> typecheck_method (arguments, body, ctx)

        | Expression.Ket values -> typecheck_ket (values,ctx)
        | Expression.KetAll size -> typecheck_ketall (size, ctx)

        | Expression.CallMethod (method, args) -> typecheck_callmethod (method, args, ctx)

        | Expression.Equals (left, right) -> typecheck_equals (left, right, ctx)
        | Expression.LessThan (left, right) -> typecheck_lessthan (left, right, ctx)

        | Expression.Join _

        | Expression.Project _
        | Expression.Block _
        | Expression.If _
        | Expression.Summarize _

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
                        "All elements in a set must be of the same type." |> Error

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

    and typecheck_range (start, stop, ctx) =
        typecheck(start, ctx)
        ==> fun (start, ctx) ->
            match start with 
            | Classic (start, Type.Int) ->
                typecheck (stop, ctx)
                ==> fun (stop, ctx) ->
                match stop with 
                | Classic (stop, Type.Int) ->
                    (Classic (C.Range (start, stop), Type.Set Type.Int), ctx) |> Ok
                | _ ->
                    $"Stop must be an int expression, got: {stop}" |> Error
            | _ ->
                $"Start must be an int expression, got: {start}" |> Error

    and typecheck_method (arguments, body, ctx) =
        let ctx =
            arguments 
            |> add_to_context ctx
        typecheck (body, ctx)
        ==> fun (body, ctx) ->
            let argNames = arguments |> List.map (fun a -> (fst a))
            let argTypes = arguments |> List.map (fun a -> (snd a))
            match body with
            | Classic (_, t) ->
                (Classic (C.Method (argNames, body), Type.Method (argTypes, AnyType.Type t)), ctx) |> Ok
            | Quantum (_, qt) ->
                (Classic (C.Method (argNames, body), Type.Method (argTypes, AnyType.QType qt)), ctx) |> Ok

    and typecheck_ket (value, ctx) =
        typecheck (Expression.Set value, ctx)
        ==> fun (value, ctx) ->
            make_q value
            ==> fun value -> (Quantum value, ctx) |> Ok

    and typecheck_add (left, right, ctx) =
        typecheck(left, ctx)
        ==> fun (left, ctx) ->
            typecheck (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | Classic _, Classic _ -> typecheck_add_classic (left, right, ctx)
                | _ -> typecheck_add_quantum (left, right, ctx)

    and typecheck_add_classic (left, right, ctx) =
        unzip_classic [left; right]
        ==> fun (values, types) ->
            match (types) with
            | [Type.Int;  Type.Int] -> (Classic (C.Add (values.[0], values.[1]), Type.Int), ctx) |> Ok
            | _ -> $"Add can only be applied to int expressions" |> Error

    and typecheck_add_quantum (left, right, ctx) =
        make_q left
        ==> fun (l, lt) ->
            make_q right
            ==> fun (r, rt) ->
                match (lt, rt) with
                | QType.Ket [Type.Int],  QType.Ket[Type.Int] -> (Quantum (Q.Add (Q.Join (l, r)), QType.Ket [Type.Int]), ctx) |> Ok
                | _ -> $"Quantum addition can only be applied to int Kets" |> Error

    and typecheck_multiply (left, right, ctx) =
        typecheck(left, ctx)
        ==> fun (left, ctx) ->
            typecheck (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | Classic _, Classic _ -> typecheck_multiply_classic (left, right, ctx)
                | _ -> typecheck_multiply_quantum (left, right, ctx)

    and typecheck_multiply_classic (left, right, ctx) =
        unzip_classic [left; right]
        ==> fun (values, types) ->
            match (types) with
            | [Type.Int;  Type.Int] -> (Classic (C.Multiply (values.[0], values.[1]), Type.Int), ctx) |> Ok
            | _ -> $"Multiply can only be applied to int expressions" |> Error

    and typecheck_multiply_quantum (left, right, ctx) =
        make_q left
        ==> fun (l, lt) ->
            make_q right
            ==> fun (r, rt) ->
                match (lt, rt) with
                | QType.Ket [Type.Int],  QType.Ket[Type.Int] -> (Quantum (Q.Multiply (Q.Join (l, r)), QType.Ket [Type.Int]), ctx) |> Ok
                | _ -> $"Quantum multiplication can only be applied to int Kets" |> Error


    and typecheck_equals (left, right, ctx) =
        typecheck(left, ctx)
        ==> fun (left, ctx) ->
            typecheck (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | Classic _, Classic _ -> typecheck_equals_classic (left, right, ctx)
                | _ -> typecheck_equals_quantum (left, right, ctx)

    and typecheck_equals_classic (left, right, ctx) =
        unzip_classic [left; right]
        ==> fun (values, types) ->
            match (types) with
            | [Type.Int;  Type.Int] -> (Classic (C.Equals (values.[0], values.[1]), Type.Bool), ctx) |> Ok
            | _ -> $"== can only be applied to int expressions" |> Error

    and typecheck_equals_quantum (left, right, ctx) =
        make_q left
        ==> fun (l, lt) ->
            make_q right
            ==> fun (r, rt) ->
                match (lt, rt) with
                | QType.Ket [Type.Int],  QType.Ket[Type.Int] -> (Quantum (Q.Equals (Q.Join (l, r)), QType.Ket [Type.Bool]), ctx) |> Ok
                | _ -> $"Quantum == can only be applied to int Kets" |> Error


    and typecheck_lessthan (left, right, ctx) =
        typecheck(left, ctx)
        ==> fun (left, ctx) ->
            typecheck (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | Classic (left, Type.Int), Classic (right, Type.Int) ->
                    (Classic (C.LessThan (left, right), Type.Bool), ctx) |> Ok
                | _ -> $"Both expressions for < must be int. Got {left} < {right}" |> Error


    and typecheck_callmethod (method, args, ctx) =
        let any_type e = e |> Ok

        // typecheck the method name:
        typecheck(method, ctx)
        ==> fun (method, ctx) ->
            // If the variable is associated with a method:
            match method with
            | Classic (m, Type.Method (argTypes, rType)) ->
                // typecheck the list of arguments to pass:
                typecheck_expression_list any_type (args, ctx)
                ==> fun (args, ctx) ->
                    // verify that the types of the arguments we'll be passing 
                    // match the expected type of the arguments
                    let argTypes' = args |> List.map (function 
                        | Classic (_, t) -> AnyType.Type t 
                        | Quantum (_, t) -> AnyType.QType t)
                    if argTypes = argTypes' then
                        // Everything matches. Check the return type
                        // to decide if it's a classical or a quantum method:
                        match rType with
                        | AnyType.Type t -> 
                            (Classic (C.CallMethod (m, args), t), ctx) |> Ok
                        | AnyType.QType qt -> 
                            (Quantum (Q.CallMethod (m, args), qt), ctx) |> Ok
                    else
                        $"Arguments type missmatch. Expected {argTypes} got {argTypes'}" |> Error
            | _ -> $"Invalid expression: {method}" |> Error
                    
    and typecheck_ketall (size, ctx) =
        typecheck (size, ctx)
        ==> fun (size, ctx) ->
            match size with 
            | Classic (v, Type.Int) ->
                (Quantum (Q.KetAll v, QType.Ket [Type.Int]), ctx) |> Ok
            | _ ->
                $"Ket size must be an int expression, got: {size}" |> Error

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
