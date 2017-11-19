namespace GlobMatcher

module AutomatonBuilder =

    open Util
    open GlobMatcher

    let makeEmpty () =
        let q0 = State (UniqueId <| shortId ())
        {Initial = [q0]; Final = [q0]; Transitions = []}

    let makeChar c =
        let q0 = State (UniqueId <| shortId ())
        let q1 = State (UniqueId <| shortId ())
        let t = {Start = q0; End = q1; Accepts = Word c}
        {Initial = [q0]; Final = [q1]; Transitions = [t]}

    let makeAnyChar () =
        let q0 = State (UniqueId <| shortId ())
        let q1 = State (UniqueId <| shortId ())
        let t = {Start = q0; End = q1; Accepts = Any}
        {Initial = [q0]; Final = [q1]; Transitions = [t]}

    let makeAnyString () =
        let q0 = State (UniqueId <| shortId ())
        let t = {Start = q0; End = q0; Accepts = Any}
        {Initial = [q0]; Final = [q0]; Transitions = [t]}

    let makeRange min max =
        let q0 = State (UniqueId <| shortId ())
        let q1 = State (UniqueId <| shortId ())
        let t = {Start = q0; End = q1; Accepts = Range (min, max)}
        {Initial = [q0]; Final = [q1]; Transitions = [t]}

    let concat {Initial = is; Final = fs; Transitions = ts} {Initial = is'; Final = fs'; Transitions = ts'} =
        let connect (f, i') = {Start = f; End = i'; Accepts = Epsilon}
        let bridges = List.allPairs fs is' |> List.map connect
        {Initial = is; Final = fs'; Transitions = ts@bridges@ts'}
    
    type NfaBuilder () =
        member x.YieldFrom m = m
        member x.For (m, f) = m |> Seq.map f |> Seq.fold concat (makeEmpty ())
        member x.Combine (m, m') = concat m m'
        member x.Delay f = f ()
        member x.Zero () = makeEmpty ()

    let nfa = NfaBuilder ()
