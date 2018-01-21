namespace StringMatcher

module RegexParser = 

    open FParsec
    open AutomatonBuilder
    open Util

    let private postfixOps = "*+?"

    let private infixOps = "|"

    let private metaChars = "()" + postfixOps + infixOps

    let private applyOp (lhs, (op, rhs)) = 
        match op with
        | None     -> lhs
        | Some '*' -> makeZeroOrMore lhs
        | Some '+' -> makeOneOrMore lhs
        | Some '?' -> makeZeroOrOne lhs
        | Some '|' -> makeAlternation (lhs, Option.get rhs)
        | Some c   -> failwithf "unknown operator '%c'" c

    let private charMatch = noneOf metaChars

    let private operand, operand' = createParserForwardedToRef ()

    let postfixOp = anyOf postfixOps |>> fun op -> Some op, None
    let infixOp = anyOf infixOps .>>. operand |>> fun (op,rhs) -> Some op, Some rhs
    let noOp = preturn (None, None)
    let private matchExpr = operand .>>. (postfixOp <|> infixOp <|> noOp) |>> applyOp

    let private submatchExpr = skipChar '(' >>. many matchExpr .>> skipChar ')'

    operand' := choice [
        submatchExpr |>> List.foldBack' connect zero
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
