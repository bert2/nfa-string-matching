namespace GlobMatcher

module AutomatonBuilder =

    open Util
    open GlobMatcher

    let concat {Initial = is; Final = fs; Transitions = ts} {Initial = is'; Final = fs'; Transitions = ts'} =
        let bridges = 
            List.allPairs fs is'
            |> List.map (fun (f,i) -> {Start = f; End = i; Accepts = Epsilon})
        {Initial = is; Final = fs'; Transitions = ts@bridges@ts'}

    let makeChar c =
        let q0 = State (UniqueId <| shortId ())
        let q1 = State (UniqueId <| shortId ())
        let t = {Start = q0; End = q1; Accepts = Word c}
        {Initial = [q0]; Final = [q1]; Transitions = [t]}