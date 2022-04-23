namespace aleph.runtime

open aleph.compiler.ast

module Classic =

    //----------------------------------
    // Expression evaluation
    //----------------------------------

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

    let (==>) (input: Result<'a,'b>) ok  =
        Result.bind ok input

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
            acc 
            ==> fun (items, ctx) -> 
                (e, ctx) |> as_literal
                ==> fun (l, ctx) -> 
                    ((List.append items l), ctx) |> Ok

        List.fold append (Ok ([], _ctx)) values
        ==> fun (v, ctx) -> 
            (Value.Tuple(v), ctx) |> Ok


    and _eval_id (id, ctx: Context) = 
        match ctx.TryFind id with
        | Some v -> (v, ctx) |> Ok
        | None -> Error $"Unknown variable: {id}"


    //----------------------------------
    // Statement evaluation
    //----------------------------------

    type StmtResult =
        | Continue of ctx: Context
        | Result of value: Value * ctx: Context
        | Error of msg: string * ctx: Context

    let (==>.) (r: StmtResult) cont  =
        match r with
        | Continue ctx -> (cont ctx)
        | _ -> r

    let rec run (p: Statement, ctx: Context) : StmtResult =
        match p with
        | Skip -> 
            Continue ctx

        | Return expr ->
            match eval (expr, ctx) with
            | Result.Ok v -> Result v
            | Result.Error msg -> Error (msg, ctx)

        | Block stmts -> 
            _run_block (stmts, ctx)

        | Let (id, e) ->
            match eval (e, ctx) with
            | Result.Ok (v, ctx) -> ctx.Add (id, v) |> Continue
            | Result.Error msg -> Error (msg, ctx)

        | _ ->
            Error ($"{p} is not implemented.", ctx)

    and _run_block (stmts, ctx) : StmtResult =
        match stmts with 
        | head :: rest ->
            run (head, ctx)
            ==>. fun ctx -> _run_block (rest, ctx)
        | [] ->
            Continue ctx
