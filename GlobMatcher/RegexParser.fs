namespace GlobMatcher

module RegexParser = 

    open FParsec
    open AutomatonBuilder

    let private postfixOps = "*+?"

    let private noop = ' '

    let private metaChars = "()" + postfixOps

    let private applyPostfix op = 
        match op with
        | '*' -> makeZeroOrMore
        | '+' -> makeOneOrMore
        | '?' -> makeZeroOrOne
        | _   -> id

    let private applyPostfixToLast (protos, op) = 
        let last = List.last protos |> applyPostfix op
        (List.front protos) @ [last]

    let private matchExpr, matchExprRef = createParserForwardedToRef<Prototype, unit> ()

    let private character = noneOf metaChars

    let private sequence = many1 matchExpr .>>. (anyOf postfixOps <|>% noop) |>> applyPostfixToLast

    let private subexpr = skipChar '(' >>. many sequence .>> skipChar ')'

    matchExprRef :=
        choice [
            subexpr   |>> List.concat |>> List.foldBack' combine zero
            character |>> makeChar
        ]

    let private parser = many sequence .>> eof |>> List.concat |>> List.foldBack' AutomatonBuilder.run empty

    let parsePattern succeed fail pattern =
        let result = CharParsers.run parser pattern
        match result with
        | Failure (msg, _, _) -> fail msg
        | Success (a, _, _)   -> succeed a

    let toAutomaton = parsePattern Result.Success Result.Failure

    let toAutomaton' = parsePattern id failwith
