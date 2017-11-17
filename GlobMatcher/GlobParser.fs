namespace GlobMatcher

type Result<'a, 'b> = Success of 'a | Failure of 'b

module GlobParser = 

    open AutomatonBuilder
    
    let private checkRangeSyntax (pattern:string) =
        match pattern.Length with
        | 0 -> Failure "Unexpected end of pattern string after start of character range."
        | 1 -> Failure "Unexpected end of pattern string after lower boundary of character range."
        | 2 -> 
            if pattern.[1] = '-' then
                Failure "Unexpected end of pattern string after minus sign '-' in character range."
            else
                Failure "Expected minus sign '-' to separate lower and upper boundary in character range."
        | 3 -> Failure "Unexpected end of pattern string after upper boundary of character range."
        | 4 when pattern.[3] <> ']' -> 
            Failure "Expected closing bracket ']' at end of character range."
        | _ -> Success pattern

    let private parseRange validatedPattern =
        match validatedPattern with
        | Success (pattern:string) -> Success (makeRange pattern.[0] pattern.[2], pattern.[4..])
        | Failure err -> Failure err

    let private checkEscapeSyntax (pattern:string) =
        match pattern.Length with
        | 0 -> Failure "Unexpected end of pattern string after escape character '\\'."
        | _ -> Success pattern

    let private parseEscaped validatedPattern =
        match validatedPattern with
        | Success (pattern:string) -> Success (makeChar pattern.[0], pattern.[1..])
        | Failure err -> Failure err

    let private parse (pattern:string) =
        match pattern.[0] with
        | '?'  -> Success (makeAnyChar (), pattern.[1..])
        | '*'  -> Success (makeAnyString (), pattern.[1..])
        | '['  -> pattern.[1..] |> checkRangeSyntax |> parseRange
        | '\\' -> pattern.[1..] |> checkEscapeSyntax |> parseEscaped
        | c    -> Success (makeChar c, pattern.[1..])

    let toAutomaton pattern =
        let rec toAutomaton' (pattern:string) automaton =
            if pattern.Length = 0 then
                Success automaton
            else
                match parse pattern with
                | Success (a, pattern') -> concat automaton a |> toAutomaton' pattern'
                | Failure err -> Failure ("Syntax error: " + err)
        toAutomaton' pattern (makeEmpty ())

    let toAutomaton' pattern =
        match toAutomaton pattern with
        | Failure msg -> failwith msg
        | Success a -> a
