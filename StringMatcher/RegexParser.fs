namespace StringMatcher

module RegexParser = 

    open FParsec
    open AutomatonBuilder
    open RegexOperatorsBuilder
    open Util

    let private postfixOps = "*+?"
    let private infixOps = "|"
    let private metaChars = "()" + postfixOps + infixOps

    let private charMatch = noneOf metaChars

    let private regexTerm matchExpr =
        let matchExprGroup = skipChar '(' >>. many matchExpr .>> skipChar ')'
        choice [
            matchExprGroup |>> List.foldBack' connect empty
            charMatch      |>> makeChar]

    let private matchExpr = 
        makeOperatorPrecedenceParser ()
        |> withPostfix "*" 1 makeZeroOrMore
        |> withPostfix "+" 1 makeOneOrMore
        |> withPostfix "?" 1 makeZeroOrOne
        |> withInfix   "|" 2 makeAlternation
        |> withTermParser regexTerm

    let private parser = 
        many matchExpr .>> eof 
        |>> List.foldBack' AutomatonBuilder.complete Final

    let parsePattern succeed fail pattern =
        let result = CharParsers.run parser pattern
        match result with
        | Failure (msg, _, _) -> fail msg
        | Success (a, _, _)   -> succeed a

    let toAutomaton = parsePattern Result.Success Result.Failure

    let toAutomaton' = parsePattern id failwith
