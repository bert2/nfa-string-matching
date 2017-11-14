namespace GlobMatcher

module GlobParser = 

    open AutomatonBuilder
    
    let private parse (pattern:string) =
        match pattern.[0] with
        | '?'  -> makeAnyChar ()                   , pattern.[1..]
        | '*'  -> makeAnyString ()                 , pattern.[1..]
        | '['  -> makeRange pattern.[1] pattern.[3], pattern.[5..]
        | '\\' -> makeChar pattern.[1]             , pattern.[2..]
        | c    -> makeChar c                       , pattern.[1..]

    let toAutomaton pattern =
        let rec toAutomaton' (pattern:string) automaton =
            match pattern.Length with
            | 0 -> automaton
            | _ -> 
                let automaton', pattern' = parse pattern
                concat automaton automaton'
                |> toAutomaton' pattern'
        toAutomaton' pattern (makeEmpty ())
