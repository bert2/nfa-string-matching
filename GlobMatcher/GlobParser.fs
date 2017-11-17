namespace GlobMatcher

type ParseResult<'a> = Success of 'a | Failure of string

module GlobParser = 

    open AutomatonBuilder
    
    let private parseRange (pattern:string) =
        Success (makeRange pattern.[0] pattern.[2]),  pattern.[4..]

    let private parseEscaped (pattern:string) =
        match pattern.Length with
        | 0 -> Failure "Unexpected end of pattern string after escape character '\\'.", pattern
        | _ -> Success (makeChar pattern.[0]), pattern.[1..]

    let private parse (pattern:string) =
        match pattern.[0] with
        | '?'  -> Success (makeAnyChar ()), pattern.[1..]
        | '*'  -> Success (makeAnyString ()), pattern.[1..]
        | '['  -> parseRange pattern.[1..]
        | '\\' -> parseEscaped pattern.[1..]
        | c    -> Success (makeChar c), pattern.[1..]

    let toAutomaton pattern =
        let rec toAutomaton' (pattern:string) automaton =
            match pattern.Length with
            | 0 -> Success automaton
            | _ -> 
                let result, pattern' = parse pattern

                match result with
                | Success a -> concat automaton a |> toAutomaton' pattern'
                | Failure err -> Failure ("Syntax error: " + err)
        toAutomaton' pattern (makeEmpty ())

    let toAutomaton' pattern =
        match toAutomaton pattern with
        | Failure msg -> failwith msg
        | Success a -> a
