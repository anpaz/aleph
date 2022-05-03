
namespace aleph.runtime

module Utils =

    let (==>) (input: Result<'a,'b>) ok  =
        Result.bind ok input

    let printTupleBody t =
        t 
        |> List.map (fun s -> s.ToString())
        |> String.concat ", "

    let printSetBody s = 
        s
        |> Set.toList
        |> List.map (function | [e] -> e.ToString() | e -> ("(" + (printTupleBody e).ToString() + ")"))
        |> String.concat ", "

