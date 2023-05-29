namespace aleph

module utils =
    open System

    let (==>) (input: Result<'a, 'b>) ok = Result.bind ok input

    let ket_counter = ref 0

    let next_id () =
        let id = System.Threading.Interlocked.Increment(ket_counter)
        id

    let int_width i =
        match i with
        | 0 -> 1
        | _ -> Math.Ceiling(Math.Log(i + 1 |> float, 2)) |> int
