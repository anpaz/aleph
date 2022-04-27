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
    type CLASSIC = string * string list * Statement
    type QUANTUM = string * string list * string * Statement

    type Value =
        | Bool      of bool
        | Int       of int
        | Tuple     of TUPLE
        | Set       of SET
        | Ket       of SET
        | Classic   of CLASSIC
        | Quantum   of QUANTUM

        override this.ToString() =
            let printLiteral= function
                | B b -> b.ToString()
                | I i -> i.ToString()
            let printSetBody s = 
                s
                |> Set.toList
                |> List.map (fun e -> (Tuple e).ToString())
                |> String.concat ", "

            match this with
            | Bool b ->
                b.ToString()
            | Int i ->
                i.ToString()
            | Tuple [t] ->
                t |> printLiteral
            | Tuple s ->
                let body =
                    s 
                    |> List.map printLiteral
                    |> String.concat ", "
                "(" + body + ")"
            | Set s ->
                let body = s |> printSetBody
                "[" + body + "]"
            | Ket k ->
                let body = k |> printSetBody
                "| " + body + " >"
            | Classic (name, _, _) ->
                "[" + name + "()]"
            | Quantum (name, _, _, _) ->
                "|" + name + "()>"

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
            evalId (id, ctx)
        | Expression.Tuple values ->
            evalTuple (values, ctx)
        | Expression.Set values ->
            evalSet (values, ctx)
        | Expression.Ket values ->
            evalKet (values, ctx)
        | Expression.Range (start, stop) ->
            evalRange (start, stop, ctx)
        | Expression.Equals (left, right) ->
            evalEquals(left, right, ctx)
        | Expression.Not value ->
            evalNot(value, ctx)
        | Expression.And values ->
            evalAnd(values, ctx)
        | Expression.Or values ->
            evalOr(values, ctx)
        | Expression.LessThan (left, right) ->
            evalLessthan (left, right, ctx)
        | Expression.Add values ->
            evalAdd(values, ctx)
        | Expression.Multiply values ->
            evalMultiply(values, ctx)

        // TODO: 
        | _ -> Error ($"{e} is not implemented")

    and private evalTuple (values: Expression list, ctx: Context) = 
        let join (left: Value) (right: Value) (T : TUPLE list -> Value) ctx =
            let getSet = function
                | Bool b -> SET [[B b]] |> Some
                | Int i -> SET [[I i]] |> Some
                | Tuple r -> SET [r] |> Some
                | Set s2 -> s2 |> Some
                | Ket k2 -> k2 |> Some
                | _ -> None
            let set1 = getSet left
            let set2 = getSet right
            match (set1, set2) with
            | (Some set1, Some set2) ->
                let result = 
                    seq {
                        for i in set1 do
                            for j in set2 ->
                                i @ j
                    }
                    |> Seq.toList
                    |> T
                (result, ctx) |> Ok
            | _ ->
                $"Not a valid join expression: ({left} , {right})" |> Error

        let append (acc: Result<Value * Context,string>) (e: Expression) =
            acc 
            ==> fun (left, ctx) ->
                eval (e, ctx)
                ==> fun (right, ctx) ->
                    match (left, right) with
                    | Ket _, _
                    | _, Ket _ ->
                        join left right (SET >> Ket) ctx
                    | Set _, _
                    | _, Set _ ->
                        join left right (SET >> Set) ctx
                    | Tuple _, _
                    | _, Tuple _ ->
                        join left right (List.head >> Tuple) ctx
                    | _ -> 
                        $"Cannot join elements: {left} - {right}" |> Error

        List.fold append (Ok (Value.Tuple [], ctx)) values


    and private evalSet (values: Expression list, ctx: Context) = 
        let append (acc: Result<Value * Context,string>) (e: Expression) =
            acc
            ==> fun (left, ctx) ->
                eval (e, ctx)
                ==> fun (right, ctx) ->
                    match (left, right) with
                    | (Set s1, Bool b) when s1.IsEmpty ->
                        (Set ([[B b]] |> SET), ctx) |> Ok
                    | (Set s1, Int i) when s1.IsEmpty ->
                        (Set ([[I i]] |> SET), ctx) |> Ok
                    | (Set s1, Tuple r) when s1.IsEmpty ->
                        (Set ([r] |> SET), ctx) |> Ok
                    | (Set s1, Set s2) when s1.IsEmpty ->
                        (Set s2, ctx) |> Ok
                    | (Set s1, Bool b) ->
                        let l = s1.MinimumElement
                        if l.Length = 1 then (Set (s1.Add [B b]), ctx) |> Ok
                        else $"All tuples must have the same length. {l} != {b}" |> Error
                    | (Set s1, Int i) ->
                        let l = s1.MinimumElement
                        if l.Length = 1 then (Set (s1.Add [I i]), ctx) |> Ok
                        else $"All tuples must have the same length. {l} != {i}" |> Error
                    | (Set s1, Tuple r) ->
                        let l = s1.MinimumElement
                        if l.Length = r.Length then (Set (s1.Add r), ctx) |> Ok
                        else $"All tuples must have the same length. {l} != {r}" |> Error
                    | (Set s1, Set s2) when s2.IsEmpty ->
                        (Set s1, ctx) |> Ok
                    | (Set s1, Set s2) ->
                        let l = s1.MinimumElement
                        let r = s2.MinimumElement
                        if l.Length = r.Length then (Set (s1 + s2), ctx) |> Ok
                        else $"All tuples must have the same length. {l} != {r}" |> Error
                    | (_, r) -> 
                        $"Invalid value for a set element: {r}" |> Error
                    
        List.fold append (Ok (Value.Set (SET []), ctx)) values

    and private evalId (id, ctx: Context) = 
        match ctx.TryFind id with
        | Some v -> (v, ctx) |> Ok
        | None -> $"Unknown variable: {id}" |> Error

    and private evalKet (values: Expression list, ctx: Context) = 
        evalSet (values, ctx)
        ==> function
            | (Set items, ctx)
            | (Ket items, ctx) -> 
                (Ket items, ctx) |> Ok
            | (v, _) -> 
                $"Invalid value for a Ket element: {v}" |> Error

    and private evalRange (start : Expression, stop : Expression, ctx: Context) = 
        let createRange s e ctx =
            let range = [s..(e - 1)] |> List.map (fun i -> [I i])
            (Value.Set (SET range), ctx) |> Ok
        eval (start, ctx)
        ==> fun (start, ctx) ->
            eval (stop, ctx)
            ==> fun (stop, ctx) ->
                match (start, stop) with
                | (Int s, Int e)
                | (Int s, Tuple [(I e)])
                | (Tuple [(I s)], Int e)
                | (Tuple [(I s)], Tuple [(I e)]) -> createRange s e ctx
                | _ ->
                    $"Invalid value for a range start/end: {start}/{stop}" |> Error


    and private evalEquals (left : Expression, right : Expression, ctx: Context) = 
        eval (left, ctx)
        ==> fun (left, ctx) ->
            eval (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | (Bool l, Bool r)
                | (Bool l, Tuple [B r])
                | (Tuple [B l], Bool r) -> (Value.Bool (l = r), ctx) |> Ok
                | (Int l, Int r)
                | (Int l, Tuple [I r])
                | (Tuple [I l], Int r) -> (Value.Bool (l = r), ctx) |> Ok
                | (Tuple l, Tuple r) -> (Value.Bool (l = r), ctx) |> Ok
                | (Set l, Set r) -> (Value.Bool (l = r), ctx) |> Ok
                | _ -> $"Invalid expression: {left} == {right}" |> Error

    and private evalNot (e, ctx: Context) = 
        eval (e, ctx)
        ==> fun (e, ctx) ->
            match e with
            | Bool b -> (Value.Bool (not b), ctx) |> Ok
            | _ -> $"Invalid expression: !{e}" |> Error


    and private evalAnd (values, ctx: Context) = 
        let next (acc: Result<Value * Context,string>) (e: Expression) =
            acc 
            ==> fun (left, ctx) ->
                eval (e, ctx)
                ==> fun (right, ctx) ->
                    match (left, right) with
                    | Bool false, _
                    | _, Bool false ->
                        (Bool false, ctx) |> Ok
                    | Bool _, Bool _ ->
                        (Bool true, ctx) |> Ok
                    | _ -> 
                        $"Invalid expression: {left} and {right}" |> Error

        List.fold next (eval (values.Head, ctx)) values.Tail


    and private evalOr (values, ctx: Context) = 
        let next (acc: Result<Value * Context,string>) (e: Expression) =
            acc 
            ==> fun (left, ctx) ->
                eval (e, ctx)
                ==> fun (right, ctx) ->
                    match (left, right) with
                    | Bool true, _
                    | _, Bool true ->
                        (Bool true, ctx) |> Ok
                    | Bool _, Bool _ ->
                        (Bool false, ctx) |> Ok
                    | _ -> 
                        $"Invalid expression: {left} or {right}" |> Error

        List.fold next (eval (values.Head, ctx)) values.Tail


    and private evalArithmetic (values: Expression list, ctx: Context) (op : Literal -> Literal -> Result<Literal, string>) = 
        let rec evalTuple s1 s2 =
            match (s1, s2) with
            | (h1::rest1), (h2::rest2) ->
                op h1 h2 |> Result.bind (fun h ->
                    match (evalTuple rest1 rest2) with
                    | Ok s -> h :: s |> Ok
                    | Result.Error msg -> Error msg)
            | [], [] ->
                [] |> Ok
            | _ -> Error "Tuples must have the same size"
        let next (acc: Result<Value * Context,string>) (e: Expression) =
            acc 
            ==> fun (left, ctx) ->
                eval (e, ctx)
                ==> fun (right, ctx) ->
                    match (left, right) with
                    | Bool l, Bool r ->
                        match (op (B l) (B r)) with
                        | Ok (B x) -> (Bool x, ctx) |> Ok
                        | _ ->  $"Cannot evaluate {l} + {r}" |> Error
                    | Int l, Int r ->
                        match (op (I l) (I r)) with
                        | Ok (I x) -> (Int x, ctx) |> Ok
                        | _ ->  $"Cannot evaluate {l} + {r}" |> Error
                    | Tuple l, Tuple r ->
                        match evalTuple l r with
                        | Ok s -> (Tuple s, ctx) |> Ok
                        | Error msg -> $"Cannot add tuple {msg}" |> Error
                    | _ -> 
                        $"Invalid expression: {left} and {right}" |> Error

        if values.Length < 2 then
            $"Need at least two operands, received: {values.Length}" |> Error
        else
            List.fold next (eval (values.Head, ctx)) values.Tail


    and private evalAdd (values: Expression list, ctx: Context) =
        let add l r =
            match (l, r) with
            | B true, B false
            | B false, B true -> B true |> Ok
            | B _, B _ -> B false |> Ok
            | I l, I r -> I (l + r) |> Ok
            | _ -> $"Tuple elements must have be of the same type: {l} != {r}." |> Error
        evalArithmetic (values, ctx) add

    and private evalMultiply (values: Expression list, ctx: Context) =
        let multiply l r =
            match (l, r) with
            | B l, B r -> B (l && r) |> Ok
            | I l, I r -> I (l * r) |> Ok
            | _ -> $"Tuple elements must have be of the same type: {l} != {r}." |> Error
        evalArithmetic (values, ctx) multiply

    and private evalLessthan (left : Expression, right : Expression, ctx: Context) = 
        eval (left, ctx)
        ==> fun (left, ctx) ->
            eval (right, ctx)
            ==> fun (right, ctx) ->
                match (left, right) with
                | (Int l, Int r)
                | (Int l, Tuple [I r])
                | (Tuple [I l], Int r) -> (Value.Bool (l < r), ctx) |> Ok
                | _ -> $"Invalid expression: {left} < {right}" |> Error

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

        | If (cond, t, f) ->
            match (eval (cond, ctx)) with
            | Result.Ok (Bool true, ctx) -> run (t, ctx)
            | Result.Ok (Bool false, ctx) -> run (f, ctx)
            | Result.Ok (_, ctx) -> ($"Invalid condition: {cond}", ctx) |> Error
            | Result.Error msg -> (msg,ctx) |> Error

        | _ ->
            Error ($"{p} is not implemented.", ctx)

    and private runBlock (stmts, ctx) : StmtResult =
        match stmts with 
        | head :: rest ->
            run (head, ctx)
            ==>. fun ctx -> runBlock (rest, ctx)
        | [] ->
            Continue ctx