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

    let makeChar c next =
        let s = State (newId (), Word c, next)
        s

    let makeAnyChar () next =
        let s1 = State (newId (), Any, next)
        s1

    let makeAnyString () next =
        let rec s1 = State (newId (), Any, s0)
        and s0 = Split (newId (), s1, next)
        s0

    let makeZeroOrMoreChar c next =
        let f = Final
        let rec s0 = Split (newId (), s1, next)
        and s1 = State (newId (), Word c, s0)
        s0

    let makeOneOrMoreChar c next =
        let rec s0 = State (newId (), Word c, s1)
        and s1 = Split (newId (), s0, next)
        s0

    let makeZeroOrOneChar c next =
        let s1 = State (newId (), Word c, next)
        let s0 = Split (newId (), s1, next)
        s0

    let makeRange (minChar, maxChar) next =
        let minChar' = min minChar maxChar
        let maxChar' = max minChar maxChar
        let s = State (newId (), Range (minChar', maxChar'), next)
        s

    let finish proto next = proto next
