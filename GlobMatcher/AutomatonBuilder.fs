namespace GlobMatcher

module AutomatonBuilder =

    open GlobMatcher

    let private newId = 
        let mutable i = -1
        fun () ->
            i <- i + 1
            i  |> string |> Id

    let empty = Final

    let makeEmpty () = empty

    let makeChar c =
        let f = Final
        let s = State (newId (), Word c, f)
        s

    let makeAnyChar () =
        let f = Final
        let s1 = State (newId (), Any, f)
        s1

    let makeAnyString () =
        let f = Final
        let rec s1 = State (newId (), Any, s0)
        and s0 = Split (newId (), s1, f)
        s0

    let makeZeroOrMoreChar c =
        let f = Final
        let rec s0 = Split (newId (), s1, f)
        and s1 = State (newId (), Word c, s0)
        s0

    let makeOneOrMoreChar c =
        let f = Final
        let rec s0 = State (newId (), Word c, s1)
        and s1 = Split (newId (), s0, f)
        s0

    let makeZeroOrOneChar c =
        let f = Final
        let s1 = State (newId (), Word c, f)
        let s0 = Split (newId (), s1, f)
        s0

    let makeRange (minChar, maxChar) =
        let minChar' = min minChar maxChar
        let maxChar' = max minChar maxChar
        let f = Final
        let s = State (newId (), Range (minChar', maxChar'), f)
        s

    let rec concat s0 s1 =
        match s0, s1 with
        | Final                   , s     -> s
        | State (id, w, Final)    , s     -> State (id, w, s)
        | State (id, w, next)     , s     -> State (id, w, concat next s)
        | Split (id, left, Final) , s     -> Split (id, left, s)
        | Split (id, left, right) , s     -> Split (id, left, concat right s)
