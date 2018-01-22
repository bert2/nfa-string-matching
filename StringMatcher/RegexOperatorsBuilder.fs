namespace StringMatcher

module RegexOperatorsBuilder =
    
    open FParsec

    type OPP<'a> = OperatorPrecedenceParser<'a, unit, unit>

    // Ugly way of telling an operators aterStringParser to immediately succeed.
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

    let withTermParser f (opp:OPP<'a>) = 
        opp.TermParser <- f opp.ExpressionParser
        opp.ExpressionParser
