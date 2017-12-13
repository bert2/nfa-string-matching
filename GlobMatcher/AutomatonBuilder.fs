namespace GlobMatcher

module AutomatonBuilder =

    open Util
    open GlobMatcher

    let private newId () = Id <| shortId ()

    let makeEmpty () = Final

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

    let makeRange min max =
        let f = Final
        let s = State (newId (), Range (min, max), f)
        s

    let rec concat s s' =
        match s, s' with
        | Final                   , s     -> s
        | State (id, w, Final)    , s     -> State (id, w, s)
        | State (id, w, next)     , s     -> State (id, w, concat next s)
        | Split (id, left, Final) , s     -> Split (id, left, s)
        | Split (id, left, right) , s     -> Split (id, left, concat right s)
    
    type NfaBuilder () =
        member x.YieldFrom m = m
        member x.For (m, f) = m |> Seq.map f |> Seq.fold concat (makeEmpty ())
        member x.Combine (m, m') = concat m m'
        member x.Delay f = f ()
        member x.Zero () = makeEmpty ()

    let nfa = NfaBuilder ()
