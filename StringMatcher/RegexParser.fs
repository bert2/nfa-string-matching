namespace StringMatcher

module RegexParser = 

    open FParsec
    open AutomatonBuilder
    open Util

    let private postfixOps = "*+?"
    let private infixOps = "|"
    let private metaChars = "()" + postfixOps + infixOps
    let private charMatch = noneOf metaChars

    let private stop = nextCharSatisfies (fun _ -> true) <|> eof
    let private oop = OperatorPrecedenceParser<_, _, _>()
    oop.AddOperator (PostfixOperator ("*", stop, 1, true, makeZeroOrMore))
    oop.AddOperator (PostfixOperator ("+", stop, 1, true, makeOneOrMore))
    oop.AddOperator (PostfixOperator ("?", stop, 1, true, makeZeroOrOne))
    oop.AddOperator (InfixOperator   ("|", stop, 2, Associativity.Left, makeAlternation))
    
    let private expr = oop.ExpressionParser

    let private submatchExpr = skipChar '(' >>. many expr .>> skipChar ')'

    oop.TermParser <- choice [
        submatchExpr |>> List.foldBack' connect empty
        charMatch    |>> makeChar]

    let private parser = 
        many expr .>> eof 
        |>> List.foldBack' AutomatonBuilder.complete Final

    let parsePattern succeed fail pattern =
        let result = CharParsers.run parser pattern
        match result with
        | Failure (msg, _, _) -> fail msg
        | Success (a, _, _)   -> succeed a

    let toAutomaton = parsePattern Result.Success Result.Failure

    let toAutomaton' = parsePattern id failwith
