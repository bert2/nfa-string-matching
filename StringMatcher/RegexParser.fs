namespace StringMatcher

module RegexParser = 

    open FParsec
    open RegexOperatorsBuilder

    let private matchExpr = 
        makeOperatorPrecedenceParser ()
        |> withPostfix "*" 2 ProtoAutom.makeZeroOrMore
        |> withPostfix "+" 2 ProtoAutom.makeOneOrMore
        |> withPostfix "?" 2 ProtoAutom.makeZeroOrOne
        |> withInfix   "|" 1 ProtoAutom.makeAlternation
        |> andWithTerms (fun matchExpr ->
            let group = skipChar '(' >>. many matchExpr .>> skipChar ')'
            let charMatch = noneOf "*+?|()"
            choice [
                group     |>> ProtoAutom.concat
                charMatch |>> ProtoAutom.makeChar])

    let private parser = many matchExpr .>> eof |>> ProtoAutom.completeAll Final

    let private parsePattern succeed fail pattern =
        let result = CharParsers.run parser pattern
        match result with
        | Failure (msg, _, _) -> fail msg
        | Success (a, _, _)   -> succeed a

    let toAutomaton = parsePattern Result.Success Result.Failure

    let toAutomaton' = parsePattern id failwith
