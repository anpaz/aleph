namespace aleph.runtime

open aleph.parser.core
open aleph.parser.quantum

open aleph.runtime.Utils
open aleph.runtime.Core

module Classic =

    let random = System.Random()

    type QuantumValue =
    | K         of SET
    | U         of arguments: string list * ket: string list * body: Expression<QuantumExpression>
        override this.ToString() =
            match this with
            | K k -> "| " + (printSetBody k) + " >"
            | U (args, ket, _) -> "(" + (args |> String.concat " ")  + ") | " + (ket |> String.concat ", ") + " => |>"

    type Value = Value<QuantumExpression, QuantumValue>
    type Expression = Expression<QuantumExpression>

    let rec AllQuantumExpressions (e, ctx) =
        match e with 
        | Unitary (args, ket, body) -> (Q (U(args, ket, body)), ctx) |> Ok
        | Ket values -> KetExpression (values, ctx)
        | Measure ket -> MeasureExpression (ket, ctx)
        | Solve ket -> SolveExpression (ket, ctx)
        | CallUnitary (name, args, ket) -> CallQuantumExpression (name, args, ket, ctx)
        | All -> $"Not implemented: {e}" |> Error

    and private KetExpression (values, ctx) = 
        eval (Expression.Set values, ctx)
        ==> function
            | (Set items, ctx)
            | (Q (K items), ctx) -> 
                (Q (K items), ctx) |> Ok
            | (v, _) -> 
                $"Invalid value for a Ket element: {v}" |> Error

    and private MeasureExpression (value: Expression, ctx) =
        eval (value, ctx)
        ==> function
        | Q (K items), ctx ->
            if items.IsEmpty then
                (Tuple [], ctx) |> Ok
            else 
                let i = int (random.NextDouble() * (double (items.Count)))
                let s = (items |> Set.toSeq |> Seq.item i)
                match s with
                | [B b] -> (Bool b, ctx) |> Ok
                | [I i] -> (Int i, ctx) |> Ok
                | s -> (Tuple s, ctx) |> Ok
        | v, _ -> $"Measure not available for {v}" |> Error

    and private SolveExpression (value: Expression, ctx) =
        eval (value, ctx)
        ==> function
        | Q (K items), ctx ->
            if items.Count > 0 then
                let length = items.MinimumElement.Length
                if (length < 2) then
                    $"Solve expects kets of size > 2. Ket size: {length}" |> Error
                else
                    let filter (t: TUPLE) =
                        match t.[t.Length - 1] with
                        | B b -> b
                        | I i -> i = -1
                    let trim (t: TUPLE) =
                        t |> List.rev |> List.tail |> List.rev
                    items |> Set.filter filter |> Set.map trim |> Ok
            else
                Set.empty |> Ok
            |> function
            | Ok selected  -> (Q (K selected), ctx) |> Ok
            | Error msg -> msg |> Error
        | v, _ -> $"Solve not available for {v}" |> Error

        
    and private CallQuantumExpression (name: string, args: Expression list, ket: Expression, ctx: Context<QuantumExpression, QuantumValue>) =
        match ctx.TryFind name with
        | Some (Q (U (argNames, ketNames, body))) ->
            // First, add arguments to variable context:
            ctx.Add (argNames, args)
            ==> fun ctx ->
                // If success,
                // check that the ket argumetn is valid and that it maps to a valid argument:
                eval (ket, ctx)
                ==> function
                    // For classical runtime, Set and Ket are equivalent:
                    //| Set ket, ctx
                    | Q (K ket), ctx ->
                        // Evaluates a single tuple on a Ket.
                        // It evaluates successfully, it adds it to the resulting Set:
                        let one (set:Result<SET,string>) (t: TUPLE) =
                            // First check if there have been errors so far:
                            set
                            ==> fun set -> 
                                // Map the input to an actual Value, and add it to the context with
                                // the ket argument name:
                                let expression = Expression.Tuple (t |> List.map (function | I i -> Expression.Int i | B b -> Expression.Bool b))
                                ctx.Add(ketNames, [expression])
                                ==> fun ctx' ->
                                    // Eval Body
                                    eval (body, ctx')
                                    ==> fun (y, ctx') ->
                                        // If successful, create the resulting Tuple
                                        // by joining the input ket (x) with the ouput (y):
                                        let x = Tuple t
                                        cross_product x y
                                        ==> fun items ->
                                            // If join successful, add all items to the set
                                            // (needs to use List.fold for this)
                                            // Finally return Ok
                                            List.fold (fun acc value -> Set.add value acc) set items 
                                            |> Ok
                        Set.fold one (SET [] |> Ok) ket
                        ==> fun set -> (Q (K set), ctx) |> Ok
                    | Tuple t, ctx ->
                        let expression = Expression.Tuple (t |> List.map (function | I i -> Expression.Int i | B b -> Expression.Bool b))
                        ctx.Add (ketNames, [expression])
                        ==> fun ctx' -> eval (body, ctx')
                    | (v,_) -> $"Expecting ket value, got: {v}" |> Error
        | Some _ -> $"Undefined quantum: {name}" |> Error
        | None ->  $"Undefined quantum: {name}" |> Error

    and toSet = function
        | Bool b -> SET [[B b]] |> Some
        | Int i -> SET [[I i]] |> Some
        | Tuple r -> SET [r] |> Some
        | Set s2 -> s2 |> Some
        | Q (K values) -> values |> Some
        | _ -> None

    and cross_product left right =
        match (toSet left, toSet right) with
        | (Some set1, Some set2) ->
            seq { for i in set1 do for j in set2 -> i @ j }
            |> Seq.toList 
            |> Ok
        | _ ->
            $"Not a valid join expression: ({left} , {right})"
            |> Error


    and join left right T ctx =
        match (toSet left, toSet right) with
        | (Some set1, Some set2) ->
            let result = seq { for i in set1 do for j in set2 -> i @ j } |> Seq.toList |> T
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
                    | Q (K _), _
                    | _, Q (K _) ->
                        join left right (SET >> K >> Q) ctx
                    | Set _, _
                    | _, Set _ ->
                        join left right (SET >> Set) ctx
                    | Tuple _, _
                    | _, Tuple _ ->
                        join left right (List.head >> Tuple) ctx
                    | _ -> 
                        $"Cannot join elements: {left} - {right}" |> Error
        List.fold append (Ok (Tuple [], ctx)) values

    and extension = { 
        new RuntimeExtension<QuantumExpression, QuantumValue> with
            override this.Eval core (e, ctx) =
                match e with
                | Expression.Q q -> AllQuantumExpressions (q, ctx) 
                | Expression.Tuple values -> TupleExpression (values, ctx)
                | _ ->
                    core (e, ctx)
    }

    and eval = (evalCore extension)

    type Context(map) =
        inherit Context<QuantumExpression, QuantumValue>(map, eval)
