namespace GlobMatcher

module RegexParser = 

    open AutomatonBuilder
    open FParsec
    
    let private token =
        choice [
            anyChar |>> makeChar
        ]

    let private parser = many token |>> List.fold concat empty

    let parsePattern succeed fail pattern =
        let result = run parser pattern
        match result with
        | Failure (msg, _, _) -> fail msg
        | Success (a, _, _)   -> succeed a

    let toAutomaton = parsePattern Result.Success Result.Failure

    let toAutomaton' = parsePattern id failwith
