namespace GlobMatcher

module RegexParser = 

    open FParsec
    open AutomatonBuilder
    
    [<Literal>]
    let private metaCharacters = "*+?"

    let private expr, expr' = createParserForwardedToRef<State -> State, unit> ()

    let private character = noneOf metaCharacters

    expr' :=
        choice [
            character .>>? skipChar '*' |>> makeZeroOrMoreChar
            character .>>? skipChar '+' |>> makeOneOrMoreChar
            character .>>? skipChar '?' |>> makeZeroOrOneChar
            character                   |>> makeChar
        ]

    let private parser = many expr .>> eof |>> List.foldBack' finish empty

    let parsePattern succeed fail pattern =
        let result = run parser pattern
        match result with
        | Failure (msg, _, _) -> fail msg
        | Success (a, _, _)   -> succeed a

    let toAutomaton = parsePattern Result.Success Result.Failure

    let toAutomaton' = parsePattern id failwith
