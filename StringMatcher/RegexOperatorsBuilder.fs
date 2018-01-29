namespace StringMatcher

module RegexOperatorsBuilder =
    
    open FParsec

    type OPP<'a> = OperatorPrecedenceParser<'a, unit, unit>

    // My ugly way of telling an operators afterStringParser to stop parsing.
    let private stop = nextCharSatisfies (fun _ -> true) <|> eof

    let makeOperatorPrecedenceParser () : OPP<'a> = OperatorPrecedenceParser<_, unit, unit>()

    let withPrefix op prec map (opp:OPP<'a>) =
        opp.AddOperator (PrefixOperator (op, stop, prec, true, map))
        opp

    let withPostfix op prec map (opp:OPP<'a>) =
        opp.AddOperator (PostfixOperator (op, stop, prec, true, map))
        opp

    let withInfix op prec map (opp:OPP<'a>) =
        opp.AddOperator (InfixOperator (op, stop, prec, Associativity.Left, map))
        opp

    let andWithTerms buildTermParser (opp:OPP<'a>) = 
        opp.TermParser <- buildTermParser opp.ExpressionParser
        opp.ExpressionParser
