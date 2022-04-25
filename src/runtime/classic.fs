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

    and private evalTuple (values: Expression list, ctx: Context) = 
        let cross_product (set1: SET) (set2: SET) =
            seq {
                for i in set1 do
                    for j in set2 ->
                        i @ j
            }
            |> SET |> Value.Set

        let append (acc: Result<Value * Context,string>) (e: Expression) =
            acc 
            ==> fun (left, ctx) ->
                eval (e, ctx)
                ==> fun (right, ctx) ->
                    match (left, right) with
                    | (Tuple l, Tuple r) ->
                        (Tuple (l @ r), ctx) |> Ok
                    | (Tuple l, Set s2) ->
                        (cross_product (SET [l]) s2, ctx) |> Ok
                    | (Set s1, Tuple r) ->
                        (cross_product s1 (SET [r]), ctx) |> Ok
                    | (Set s1, Set s2) ->
                        (cross_product s1 s2, ctx) |> Ok
                    | _ -> 
                        acc

        List.fold append (Ok (Value.Tuple [], ctx)) values


    and private evalSet (values: Expression list, ctx: Context) = 
        let append (acc: Result<Value * Context,string>) (e: Expression) =
            acc
            ==> fun (left, ctx) ->
                eval (e, ctx)
                ==> fun (right, ctx) ->
                    match (left, right) with
                    | (Set s1, Tuple r) when s1.IsEmpty ->
                        ([r] |> SET |> Set, ctx) |> Ok
                    | (Set s1, Set s2) when s1.IsEmpty ->
                        (Set s2, ctx) |> Ok
                    | (Set s1, Set s2) when s2.IsEmpty ->
                        (Set s1, ctx) |> Ok
                    | (Set s1, Tuple r) ->
                        let l = s1.MinimumElement
                        if l.Length = r.Length then (Set (s1.Add r), ctx) |> Ok
                        else $"All tuples must have the same length. {l} != {r}" |> Error
                    | (Set s1, Set s2) ->
                        let l = s1.MinimumElement
                        let r = s2.MinimumElement
                        if l.Length = r.Length then (Set (s1 + s2), ctx) |> Ok
                        else $"All tuples must have the same length. {l} != {r}" |> Error
                    | (l, r) -> 
                        "Invalid value for a set element: {r}" |> Error
                    
        List.fold append (Ok (Value.Set (SET []), ctx)) values

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
