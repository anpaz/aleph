namespace aleph.runtime

open aleph.compiler.ast

module Classic =

    type Literal = 
        | B of bool
        | I of int
    type Tuple = Literal list
    type Set = Tuple list
    type Ket = Set list
    type Classic = string * string list * Statement
    type Quantum = string * string list * string * Statement

    type Value =
        | Bool      of bool
        | Int       of int
        | Tuple     of Tuple
        | Set       of Set
        | Ket       of Ket
        | Classic   of Classic
        | Quantum   of Quantum

    type Context = Map<string, Value>

    let bind input ok  =
        match input with
        | Ok v -> Ok (ok v)
        | _ -> input


    let rec eval (e: Expression, ctx: Context) : Result<Value * Context, string> =
        match e with
        | Expression.Int i -> 
            (Value.Int i, ctx) |> Ok
        | Expression.Bool b -> 
            (Value.Bool b, ctx) |> Ok
        | Expression.Id id -> 
            _eval_id (id, ctx)
        | Expression.Tuple values -> 
            _eval_tuple (values, ctx)
        | _ -> 
            Error ($"{e} is not implemented")


    and _eval_tuple (values: Expression list, _ctx: Context) = 
        let as_literal (e, ctx) =
            match eval (e, ctx) with
            | Ok (Bool b, ctx) -> 
                ([B b], ctx) |> Ok
            | Ok (Int i, ctx) -> 
                ([I i], ctx) |> Ok
            | Ok _ -> 
                Error $"Invalid value for a tuple element: {e}"
            | Error msg ->
                Error msg

        let append (acc: Result<Literal list * Context,string>) (e: Expression) =
            match acc with
            | Ok (items, ctx) ->
                let n = as_literal (e, ctx)
                match n with
                | Ok (l, ctx) -> Ok (List.append items l, ctx)
                | _ -> n
            | _ -> acc

        let input = List.fold  append (Ok ([], _ctx)) values

        match input with
        | Ok (v, ctx) -> (Value.Tuple v, ctx) |> Ok
        | Error msg -> Error msg

    and _eval_id (id, ctx: Context) = 
        match ctx.TryFind id with
        | Some v -> (v, ctx) |> Ok
        | None -> Error $"Unassigned variable: {id}"

    type StmtResult =
        | Continue
        | Result of value: Value
        | Error of msg: string

    let rec run ctx (p: Statement) : StmtResult =
        match p with 
        | Block stmts -> _run_block ctx stmts
        | _ ->
            Error $"{p} is not implemented."

    and _run_block ctx stmts : StmtResult =
        match stmts with 
        | head :: rest ->
            let r = (run ctx head)
            match r with
            | Continue -> (_run_block ctx rest)
            | _ -> r
        | [] ->
            Error "Missing Return value."
