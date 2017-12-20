namespace GlobMatcher

module RegexParser = 

    open FParsec
    open AutomatonBuilder
    
    [<Literal>]
    let private metaCharacters = "*+?()"

    let private expr, expr' = createParserForwardedToRef<Prototype, unit> ()

    let private character = noneOf metaCharacters

    let private subexpr = skipChar '(' >>. many1 expr .>> skipChar ')'

    expr' :=
        choice [
            subexpr                     |>> List.foldBack' combine zero
            character .>>? skipChar '*' |>> makeZeroOrMoreChar
            character .>>? skipChar '+' |>> makeOneOrMoreChar
            character .>>? skipChar '?' |>> makeZeroOrOneChar
            character                   |>> makeChar
        ]

    let private parser = many expr .>> eof |>> List.foldBack' AutomatonBuilder.run empty

    let parsePattern succeed fail pattern =
        let result = CharParsers.run parser pattern
        match result with
        | Failure (msg, _, _) -> fail msg
        | Success (a, _, _)   -> succeed a

    let toAutomaton = parsePattern Result.Success Result.Failure

    let toAutomaton' = parsePattern id failwith
