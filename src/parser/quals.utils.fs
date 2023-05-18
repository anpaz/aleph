namespace aleph

module utils =
    open System

    let (==>) (input: Result<'a, 'b>) ok = Result.bind ok input

    let ket_counter = ref 0
    let next_id() =
        let id = System.Threading.Interlocked.Increment(ket_counter) * 2
        id
