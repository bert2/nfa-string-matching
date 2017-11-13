namespace GlobMatcher

module GlobParser = 

    open AutomatonBuilder
    
    let private parse c =
        match c with
        | '?' -> makeAnyChar ()
        | '*' -> makeAnyString ()
        | c -> makeChar c

    let toAutomaton (pattern:string) =
        pattern
        |> Seq.map parse
        |> Seq.fold concat (makeEmpty ())
