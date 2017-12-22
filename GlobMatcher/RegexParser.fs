namespace GlobMatcher

module RegexParser = 

    open FParsec
    open AutomatonBuilder
    open Util

    let private postfixOps = "*+?"

    let private noop = System.Char.MinValue

    let private metaChars = "()" + postfixOps

    let private applyPostfix (operand, op) = 
        match op with
        | '*' -> makeZeroOrMore operand
        | '+' -> makeOneOrMore operand
        | '?' -> makeZeroOrOne operand
        | _   -> operand

    let private charMatch = noneOf metaChars

    let private postfixOperand, postfixOperand' = createParserForwardedToRef ()

    let private postfixOperator = anyOf postfixOps <|>% noop

    let private matchExpr = postfixOperand .>>. postfixOperator |>> applyPostfix

    let private submatchExpr = skipChar '(' >>. many matchExpr .>> skipChar ')'

    postfixOperand' := choice [
        submatchExpr |>> List.foldBack' combine zero
        charMatch    |>> makeChar]

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
