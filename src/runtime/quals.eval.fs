namespace aleph.quals.runtime

open aleph.quals.parser.ast

module Eval =

    let (==>) (input: Result<'a, 'b>) ok = Result.bind ok input

    type IUniverse =
        interface
        end

    type QPU =
        abstract Prepare: Expression list -> Result<IUniverse, string>
        abstract Measure: IUniverse -> Result<int list, string>

    type EvalContext = {
        qpu : QPU
    }

    let prepare ctx (kets: Expression list) : Result<IUniverse, string> =
        let qpu = ctx.qpu
        qpu.Prepare kets

    let sample ctx (kets: Expression list) : Result<int list, string> =
        prepare ctx kets
        ==> fun u ->
            let qpu = ctx.qpu
            qpu.Measure u

