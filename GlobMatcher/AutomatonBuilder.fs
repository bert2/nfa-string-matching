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
        let t = {Start = q0; End = q0; Accepts = Range (min, max)}
        {Initial = [q0]; Final = [q0]; Transitions = [t]}

    let concat {Initial = is; Final = fs; Transitions = ts} {Initial = is'; Final = fs'; Transitions = ts'} =
        let connect (f, i') = {Start = f; End = i'; Accepts = Epsilon}
        let bridges = List.allPairs fs is' |> List.map connect
        {Initial = is; Final = fs'; Transitions = ts@bridges@ts'}
