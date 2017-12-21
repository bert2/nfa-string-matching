namespace GlobMatcher

module RegexParser = 

    open FParsec
    open AutomatonBuilder

    let private postfixOps = "*+?"

    let private noop = ' '

    let private metaChars = "()" + postfixOps

    let private applyPostfix (proto, op) = 
        match op with
        | '*' -> makeZeroOrMore proto
        | '+' -> makeOneOrMore proto
        | '?' -> makeZeroOrOne proto
        | _   -> proto

    let private matchExpr, matchExpr' = createParserForwardedToRef<Prototype, unit> ()

    let private charMatch = noneOf metaChars

    let private postfixOp = anyOf postfixOps <|>% noop

    let private postfixedMatchExpr = matchExpr .>>. postfixOp |>> applyPostfix

    let private submatchExpr = skipChar '(' >>. many postfixedMatchExpr .>> skipChar ')'

    matchExpr' :=
        choice [
            submatchExpr |>> List.foldBack' combine zero
            charMatch    |>> makeChar
        ]

    let private parser = 
        many postfixedMatchExpr 
        .>> eof 
        |>> List.foldBack' AutomatonBuilder.run empty

    let parsePattern succeed fail pattern =
        let result = CharParsers.run parser pattern
        match result with
        | Failure (msg, _, _) -> fail msg
        | Success (a, _, _)   -> succeed a

    let toAutomaton = parsePattern Result.Success Result.Failure

    let toAutomaton' = parsePattern id failwith
