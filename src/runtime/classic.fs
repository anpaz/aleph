// fsharplint:disable-next-line NamespaceNames
namespace aleph.runtime

open aleph.compiler.ast

module Classic =

    //----------------------------------
    // Expression evaluation
    //----------------------------------

    type Literal = 
        | B of bool
        | I of int
    type TUPLE = Literal list
    type SET = Set<TUPLE>
    type KET = SET list
    type CLASSIC = string * string list * Statement
    type QUANTUM = string * string list * string * Statement

    type Value =
        | Tuple     of TUPLE
        | Set       of SET
        | Ket       of KET
        | Classic   of CLASSIC
        | Quantum   of QUANTUM

    type Context = Map<string, Value>

    let (==>) (input: Result<'a,'b>) ok  =
        Result.bind ok input

    let rec eval (e: Expression, ctx: Context) : Result<Value * Context, string> =
        match e with
        | Expression.Int i -> 
            (Value.Tuple [I i], ctx) |> Ok
        | Expression.Bool b -> 
            (Value.Tuple [B b], ctx) |> Ok
        | Expression.Id id -> 
            evalId (id, ctx)
        | Expression.Tuple values ->
            evalTuple (values, ctx)
        | Expression.Set values ->
            evalSet (values, ctx)
        | _ -> 
            Error ($"{e} is not implemented")

    and private evalTuple (values: Expression list, _ctx: Context) = 
        let asLiteral (e, ctx) =
            match eval (e, ctx) with
            | Ok (Tuple b, ctx) -> 
                (b, ctx) |> Ok
            | Ok _ -> 
                Error $"Invalid value for a tuple element: {e}"
            | Error msg ->
                Error msg

        let append (acc: Result<Literal list * Context,string>) (e: Expression) =
            acc 
            ==> fun (items, ctx) -> 
                (e, ctx) |> asLiteral
                ==> fun (l, ctx) -> 
                    ((List.append items l), ctx) |> Ok

        List.fold append (Ok ([], _ctx)) values
        ==> fun (v, ctx) -> 
            (Value.Tuple(v), ctx) |> Ok


    and private evalSet (values: Expression list, _ctx: Context) = 
        let asTuple (e, ctx) : Result<TUPLE * Context, string>=
            match eval (e, ctx) with
            | Ok (Tuple b, ctx) -> 
                (b, ctx) |> Ok
            | Ok _ -> 
                Error $"Invalid value for a set element: {e}"
            | Error msg ->
                Error msg

        let append (acc: Result<Set<Literal list> * Context,string>) (e: Expression) =
            acc 
            ==> fun (items, ctx) -> 
                (e, ctx) |> asTuple
                ==> fun (l, ctx) ->
                    ((Set.add l items), ctx) |> Ok

        List.fold append (Ok (Set.empty, _ctx)) values
        ==> fun (v, ctx) -> 
            (Value.Set(v), ctx) |> Ok

    and private evalId (id, ctx: Context) = 
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
            runBlock (stmts, ctx)

        | Let (id, e) ->
            match eval (e, ctx) with
            | Result.Ok (v, ctx) -> ctx.Add (id, v) |> Continue
            | Result.Error msg -> Error (msg, ctx)

        | _ ->
            Error ($"{p} is not implemented.", ctx)

    and private runBlock (stmts, ctx) : StmtResult =
        match stmts with 
        | head :: rest ->
            run (head, ctx)
            ==>. fun ctx -> runBlock (rest, ctx)
        | [] ->
            Continue ctx
