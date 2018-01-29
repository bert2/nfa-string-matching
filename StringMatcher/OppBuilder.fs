namespace StringMatcher

module OppBuilder =
    
    open FParsec

    // Split the OPP into two parts in order to deal with implicit operators (see
    // https://stackoverflow.com/questions/29322892).
    type OppBuilder<'a> = 
        { 
            High: OperatorPrecedenceParser<'a, unit, unit>; 
            Low: OperatorPrecedenceParser<'a, unit, unit>; 
            ImplicitOpMap: 'a -> 'a -> 'a 
        }

    // My ugly way of telling an operator's afterStringParser to stop parsing.
    let private stop = nextCharSatisfies (fun _ -> true) <|> eof

    let private getHighOrLow prec opBuilder = 
        if (prec > 0) then opBuilder.High else opBuilder.Low

    let makeOpPrecParserWithImplicit implicitOpMap = 
        { 
            High = OperatorPrecedenceParser<_, unit, unit>(); 
            Low  = OperatorPrecedenceParser<_, unit, unit>();
            ImplicitOpMap = implicitOpMap
        }

    let withPrefix op prec map opBuilder =
        let op = PrefixOperator (op, stop, prec + 1000, true, map)
        (opBuilder |> getHighOrLow prec).AddOperator op
        opBuilder

    let withPostfix op prec map opBuilder =
        let op = PostfixOperator (op, stop, prec + 1000, true, map)
        (opBuilder |> getHighOrLow prec).AddOperator op
        opBuilder

    let withInfix op prec map opBuilder =
        let op = InfixOperator (op, stop, prec + 1000, Associativity.Left, map)
        (opBuilder |> getHighOrLow prec).AddOperator op
        opBuilder

    let andWithTerms buildTermParser opBuilder = 
        opBuilder.High.TermParser <- buildTermParser opBuilder.Low.ExpressionParser
        opBuilder.Low.TermParser  <- many1 opBuilder.High.ExpressionParser |>> List.reduce opBuilder.ImplicitOpMap
        opBuilder.Low.ExpressionParser
