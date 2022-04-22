namespace aleph.runtime.eval

type Literal<'b,'i> =
    | B of 'b
    | I of 'i

type Tuple<'b, 'i> = 
    Tuple of Literal<'b,'i> list

type Set<'b, 'i> = 
    Set of Tuple<'b,'i> list

type Value<'b,'i, 'k, 'f, 'q> =
    | Bool of 'b
    | Int of 'i
    | Tuple of Tuple<'b,'i>
    | Set of Set<'b,'i>
    | Ket of 'k
    | FuncClassic of 'f
    | FuncQuantum of 'q
