namespace aleph.runtime

open aleph.parser.core
open aleph.parser.quantum

open aleph.runtime.Utils
open aleph.runtime.Core

module Classic =

    let random = System.Random()

    type QuantumValue =
    | K         of SET
    | U         of string list * string * Expression<QuantumExpression>
        override this.ToString() =
            match this with
            | K k -> "| " + (printSetBody k) + " >"
            | U (args, ket, _) -> "(" + (args |> String.concat " ")  + ") | " + ket + " => |>"

    type Value = Value<QuantumExpression, QuantumValue>
    type Context = Context<QuantumExpression, QuantumValue>
    type Expression = Expression<QuantumExpression>

    let rec evalQuantum (e, ctx) =
        match e with 
        | Ket values -> evalKet (values, ctx)
        | Measure ket -> evalMeasure (ket, ctx)
        | Solve ket -> evalSolve (ket, ctx)
        | Unitary _
        | CallUnitary _ 
        | All -> $"Not implemented: {e}" |> Error

    and private evalKet (values, ctx) = 
        eval (Expression.Set values, ctx)
        ==> function
            | (Set items, ctx)
            | (Q (K items), ctx) -> 
                (Q (K items), ctx) |> Ok
            | (v, _) -> 
                $"Invalid value for a Ket element: {v}" |> Error

    and private evalMeasure (value: Expression, ctx: Context) =
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

    and private evalSolve (value: Expression, ctx: Context) =
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

    and extension = { 
        new RuntimeExtension<QuantumExpression, QuantumValue> with
            override this.Eval (e, ctx) = evalQuantum(e, ctx)

            override this.ToSet v = 
                match v with 
                | K values -> values |> Some
                | _ -> None

            override this.FromSet s = (K s)
    }

    and eval = (evalCore extension)
