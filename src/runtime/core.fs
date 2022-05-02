// fsharplint:disable-next-line NamespaceNames
namespace aleph.runtime

open aleph.compiler.ast


module Core =
    //----------------------------------
    // Expression evaluation
    //----------------------------------

    type Literal = 
        | B of bool
        | I of int
        override this.ToString() =
            match this with
            | B b -> b.ToString()
            | I i -> i.ToString()

    type TUPLE = Literal list
    type SET = Set<TUPLE>
    type METHOD<'E> = string list * Expression<'E>

    type Value<'E, 'V> =
        | Bool      of bool
        | Int       of int
        | Tuple     of TUPLE
        | Set       of SET
        | Method    of METHOD<'E>
        | Q         of 'V

        override this.ToString() =
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
                t.ToString()
            | Tuple s ->
                let body =
                    s 
                    |> List.map (fun s -> s.ToString())
                    |> String.concat ", "
                "(" + body + ")"
            | Set s ->
                let body = s |> printSetBody
                "[" + body + "]"
            | Method (args,  _) ->
                "classic (" + (args |> String.concat " ") + ")"
            | Q e ->
                e.ToString()

    type Context<'E,'V> = Map<string, Value<'E,'V>>

    let (==>) (input: Result<'a,'b>) ok  =
        Result.bind ok input

    let eval<'E,'V>  (extension: 'E * Context<'E,'V> -> Result<Value<'E,'V> * Context<'E,'V>, string>) (e: Expression<'E>, ctx: Context<'E,'V>): Result<Value<'E,'V> * Context<'E,'V>, string> =
        let rec eval (e: Expression<'E>, ctx: Context<'E,'V>) : Result<Value<'E,'V> * Context<'E,'V>, string> =
            match e with
            | Expression.Int i -> 
                (Value.Int i, ctx) |> Ok
            | Expression.Bool b -> 
                (Value.Bool b, ctx) |> Ok
            | Expression.Id id -> 
                IdExpression (id, ctx)
            | Expression.Tuple values ->
                TupleExpression (values, ctx)
            | Expression.Set values ->
                SetExpression (values, ctx)
            | Expression.Range (start, stop) ->
                RangeExpression (start, stop, ctx)
            | Expression.Not value ->
                NotExpression(value, ctx)
            | Expression.Equals (left, right) ->
                EqualsExpression(left, right, ctx)
            | Expression.LessThan (left, right) ->
                LessThanExpression (left, right, ctx)
            | Expression.Block (stmt, value) ->
                BlockExpression(stmt, value, ctx)
            | Expression.And values ->
                AndExpression(values, ctx)
            | Expression.Or values ->
                OrExpression(values, ctx)
            | Expression.Add values ->
                AddExpression(values, ctx)
            | Expression.Multiply values ->
                MultiplyExpression(values, ctx)
            | Expression.If (cond, t, f) ->
                IfExpression (cond, t, f, ctx)
            | Expression.Summarize (name, enumeration, operation, body) ->
                SummarizeExpression (name, enumeration, operation, body, ctx)
            | Expression.Project (values, indices) ->
                ProjectExpression (values, indices, ctx)
            | Expression.CallMethod (name, args) ->
                CallExpression (name, args, ctx)
            | Expression.Q v ->
                extension (v, ctx)

        and join left right T ctx =
            let getSet = function
                | Bool b -> SET [[B b]] |> Some
                | Int i -> SET [[I i]] |> Some
                | Tuple r -> SET [r] |> Some
                | Set s2 -> s2 |> Some
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

        and TupleExpression (values, ctx) = 
            let append previous next =
                previous 
                ==> fun (left, ctx) ->
                    eval (next, ctx)
                    ==> fun (right, ctx) ->
                        match (left, right) with
                        | Set _, _
                        | _, Set _ ->
                            join left right (SET >> Set) ctx
                        | Tuple _, _
                        | _, Tuple _ ->
                            join left right (List.head >> Tuple) ctx
                        | _ -> 
                            $"Cannot join elements: {left} - {right}" |> Error

            List.fold append (Ok (Value.Tuple [], ctx)) values


        and SetExpression (values, ctx) = 
            let append previous next =
                previous
                ==> fun (left, ctx) ->
                    eval (next, ctx)
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

        and IdExpression (id, ctx) = 
            match ctx.TryFind id with
            | Some v -> (v, ctx) |> Ok
            | None -> $"Unknown variable: {id}" |> Error

        and RangeExpression (start, stop, ctx) = 
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
                        $"Invalid value for a range start..end: {start}..{stop}" |> Error


        and EqualsExpression (left, right, ctx) = 
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

        and NotExpression (e, ctx) = 
            eval (e, ctx)
            ==> fun (e, ctx) ->
                match e with
                | Bool b -> (Value.Bool (not b), ctx) |> Ok
                | _ -> $"Invalid expression: !{e}" |> Error

        and AndExpression (values, ctx) = 
            let apply previous next =
                previous 
                ==> fun (left, ctx) ->
                    eval (next, ctx)
                    ==> fun (right, ctx) ->
                        match (left, right) with
                        | Bool false, _
                        | _, Bool false ->
                            (Bool false, ctx) |> Ok
                        | Bool _, Bool _ ->
                            (Bool true, ctx) |> Ok
                        | _ -> 
                            $"Invalid expression: {left} and {right}" |> Error

            List.fold apply (eval (values.Head, ctx)) values.Tail


        and OrExpression (values, ctx) = 
            let apply previous next =
                previous 
                ==> fun (left, ctx) ->
                    eval (next, ctx)
                    ==> fun (right, ctx) ->
                        match (left, right) with
                        | Bool true, _
                        | _, Bool true ->
                            (Bool true, ctx) |> Ok
                        | Bool _, Bool _ ->
                            (Bool false, ctx) |> Ok
                        | _ -> 
                            $"Invalid expression: {left} or {right}" |> Error

            List.fold apply (eval (values.Head, ctx)) values.Tail


        and LessThanExpression (left, right, ctx) = 
            eval (left, ctx)
            ==> fun (left, ctx) ->
                eval (right, ctx)
                ==> fun (right, ctx) ->
                    match (left, right) with
                    | (Int l, Int r)
                    | (Int l, Tuple [I r])
                    | (Tuple [I l], Int r) -> (Value.Bool (l < r), ctx) |> Ok
                    | _ -> $"Invalid expression: {left} < {right}" |> Error


        and ArithmeticExpression (values : Expression<'E> list, ctx) (op : Literal -> Literal -> Result<Literal, string>) = 
            let rec evalTuple s1 s2 =
                match (s1, s2) with
                | (h1::rest1), (h2::rest2) ->
                    op h1 h2 |> Result.bind (fun h ->
                        match (evalTuple rest1 rest2) with
                        | Ok s -> h :: s |> Ok
                        | Result.Error msg -> Error msg)
                | [], [] ->
                    [] |> Ok
                | _ -> Error "tuples must have the same size"
            let apply previous next =
                previous 
                ==> fun (left, ctx) ->
                    eval (next, ctx)
                    ==> fun (right, ctx) ->
                        match (left, right) with
                        | Bool l, Bool r ->
                            match (op (B l) (B r)) with
                            | Ok (B x) -> (Bool x, ctx) |> Ok
                            | Ok (I x) -> (Int x, ctx) |> Ok
                            | Error msg -> $"Cannot evaluate: {msg}" |> Error
                        | Tuple [I l], Tuple [I r]
                        | Tuple [I l], Int r
                        | Int l, Tuple [I r]
                        | Int l, Int r ->
                            match (op (I l) (I r)) with
                            | Ok (B x) -> (Bool x, ctx) |> Ok
                            | Ok (I x) -> (Int x, ctx) |> Ok
                            | Error msg -> $"Cannot evaluate: {msg}" |> Error
                        | Tuple l, Tuple r ->
                            match evalTuple l r with
                            | Ok s -> (Tuple s, ctx) |> Ok
                            | Error msg -> $"Cannot evaluate: {msg}" |> Error
                        | x, y -> 
                            $"Invalid operands: {left} and {right}" |> Error

            if values.Length < 2 then
                $"Need at least two operands, received: {values.Length}" |> Error
            else
                List.fold apply (eval (values.Head, ctx)) values.Tail


        and AddExpression (values, ctx) =
            let add l r =
                match (l, r) with
                | B true, B false
                | B false, B true -> B true |> Ok
                | B _, B _ -> B false |> Ok
                | I l, I r -> I (l + r) |> Ok
                | _ -> $"tuple elements must have be of the same type: {l} != {r}" |> Error
            ArithmeticExpression (values, ctx) add

        and MultiplyExpression (values, ctx) =
            let multiply l r =
                match (l, r) with
                | B l, B r -> B (l && r) |> Ok
                | I l, I r -> I (l * r) |> Ok
                | _ -> $"tuple elements must have be of the same type: {l} != {r}" |> Error
            ArithmeticExpression (values, ctx) multiply

        and IfExpression (cond, t, f, ctx) =
            eval (cond, ctx)
            ==> fun (cond, ctx) ->
                match cond with
                | Bool true -> eval (t, ctx)
                | Bool false -> eval (f, ctx)
                | _ -> $"Invalid condition: {cond}" |> Error

        and ProjectExpression (value, index, ctx) =
            let indices previous next =
                previous
                |> Result.bind (fun (previous, ctx) ->
                    eval (next, ctx)
                    ==> fun (next, ctx) ->
                        match next with
                        | Int next
                        | Tuple [I next] -> (previous @ [ next ], ctx) |> Ok
                        | _ -> "all indices must be of type int: {next}" |> Error
                )

            match List.fold indices (([], ctx) |> Ok) index with
            | Ok (indices, ctx) ->
                try
                    match eval (value, ctx) with
                    | Ok (Tuple t, ctx) ->
                        if indices.Length = 1 then
                            match t.[indices.[0]] with 
                            | I i -> (Int i, ctx) |> Ok
                            | B b -> (Bool b, ctx) |> Ok
                        else
                            (Tuple [ for i in indices do yield t.[i] ], ctx) |> Ok
                    | Ok (Set t, ctx) ->
                        let elems = [ for e in t -> [for i in indices do yield e.[i] ]] |> SET
                        (Set elems, ctx) |> Ok
                    | Ok _ -> $"Unable to project from {value}" |> Error
                    | Error msg -> $"Unable to project: {msg}" |> Error
                with
                | :? System.ArgumentException -> $"Index in project outside of range" |> Error
            | Error msg -> $"Invalid indices: {msg}" |> Error

        and addArgsToContext (argNames: string list) (args: Expression<'E> list) ctx =
            let one previous arg =
                previous 
                ==> fun (previous, ctx) ->
                    eval (arg, ctx)
                    ==> fun (arg, ctx) -> (previous @ [arg], ctx) |> Ok

            if argNames.Length = args.Length then
                List.fold one (([], ctx) |> Ok) args
                ==> fun (args, ctx) ->
                    let args = List.zip argNames args |> Map
                    let ctx =  Map.fold (fun acc key value -> Map.add key value acc) ctx args
                    ctx |> Ok
            else
                $"Invalid arguments: expects {argNames.Length}, got {args.Length}" |> Error

        and BlockExpression (stmts, value, ctx) =
            let rec evalStatements previous next =
                previous
                ==> fun ctx ->
                    match next with 
                    | head :: rest ->
                        evalStmt (head, ctx)
                        ==> fun ctx -> 
                            evalStatements (ctx|> Ok) rest
                    | [] -> ctx |> Ok

            evalStatements (ctx |> Ok) stmts
            ==> fun ctx' ->
                eval (value, ctx')
                ==> fun (v, _) -> (v, ctx) |> Ok

        and CallExpression (name, args, ctx) =
            match ctx.TryFind name with
            | Some (Method (argNames, body)) ->
                addArgsToContext argNames args ctx
                ==> fun ctx -> eval (body, ctx)
            | Some _ -> $"Undefined method: {name}" |> Error
            | None ->  $"Undefined method: {name}" |> Error


        and enumerate (value, ctx) =
            match (eval (value, ctx)) with
            | Result.Ok (Int i, _) -> [Int i] |> Ok
            | Result.Ok (Bool b, _) -> [Bool b] |> Ok
            | Result.Ok (Tuple t, _) -> 
                t |> List.map (function | B b -> Bool b | I i -> Int i) |> Ok
            | Result.Ok (Set s, _) ->
                s 
                |> Set.toList
                |> List.map Value.Tuple
                |> Ok
            | Result.Ok (v, _) -> $"Invalid enumeration: {v}" |> Error
            | Result.Error msg -> msg |> Error


        and SummarizeExpression (name, enumeration, operation, body, ctx) =
            // Get all elements from the enumeration
            enumerate (enumeration, ctx)
            ==> fun elements ->
                // Get all values to aggregate by evaluating the body 
                // for each element in the enumeration
                let oneValue previous next =
                    previous
                    ==> fun previous ->
                        let ctx' = ctx.Add (name, next)
                        eval (body, ctx')
                        ==> fun (next, _) ->
                            (previous @ [next]) |> Ok
                List.fold oneValue ([] |> Ok) elements
                ==>  fun values ->
                    // Aggregate all the values.
                    // If only one value, return that
                    if values.Length = 1 then
                        (values.Head, ctx) |> Ok
                    else
                        // If more than one value, expect that "operation" points to a binary operation
                        // return the value of calling the method recursively for each element.
                        let one previous next =
                            previous
                            ==> fun previous ->
                                let ctx' = ctx.Add("__p__", previous).Add("__n__", next)
                                CallExpression (operation, [Id "__p__"; Id "__n__"], ctx') 
                                ==> fun (v, _) -> v |> Ok
                        List.fold one (values.Head |> Ok) values.Tail
                        ==> fun value -> (value, ctx) |> Ok

        and evalStmt (p, ctx) =
            match p with
            | Let (id, e) -> eval (e, ctx) ==> fun (v, ctx) -> ctx.Add (id, v) |> Ok
            | Print (msg, values) -> Print (msg, values, ctx)

        and Print (msg, values, ctx) =
            let iterate result next =
                result 
                ==> fun (result, ctx) ->
                    eval (next, ctx)
                    ==> fun (next, ctx) -> (result @ [ next.ToString() ], ctx) |> Ok
            List.fold iterate (([], ctx) |> Ok) values
            ==> fun (values, ctx) ->
                ([msg; (values |> String.concat "; ")] |> String.concat ": ")
                |> printfn "%s" 
                ctx |> Ok

        eval (e, ctx)