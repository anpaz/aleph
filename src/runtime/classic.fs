namespace aleph.runtime

open aleph.compiler.ast
open aleph.runtime

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

    let rec eval (ctx: Context) (e: Expression) : Result<Value, string> =
        match e with
        | Expression.Int i -> Ok (Value.Int i)
        | Expression.Bool b -> Ok (Value.Bool b)
        | Id id -> _eval_id ctx id
        | _ -> Error ($"{e} is not implemented")

    and _eval_id (ctx: Context) id = 
        match ctx.TryFind id with
        | Some v -> Ok v
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
