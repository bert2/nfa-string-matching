namespace StringMatcher

module OppBuilder =
    
    open FParsec

    // Split the OPP into two parts in order to deal with implicit operators (see
    // https://stackoverflow.com/questions/29322892).
    type OppBuilder<'a> = 
        { 
            High: OperatorPrecedenceParser<'a, unit, unit>; 
            Low: OperatorPrecedenceParser<'a, unit, unit>; 
            ImplicitOpPrec: int;
            ImplicitOpMap: 'a -> 'a -> 'a 
        }

    // My ugly way of telling an operator's afterStringParser to stop parsing.
    let private stop = nextCharSatisfies (fun _ -> true) <|> eof

    let private addOp (op:Operator<_, _, _>) opBuilder = 
        if opBuilder.ImplicitOpPrec <= 0 
        then failwith "Define the implicit operator before any other operators."

        let opp = 
            if (op.Precedence > opBuilder.ImplicitOpPrec) 
            then opBuilder.High 
            else opBuilder.Low
        opp.AddOperator op
        opBuilder

    let makeOperatorPrecedenceParser () = 
        { 
            High = OperatorPrecedenceParser<_, _, _>(); 
            Low  = OperatorPrecedenceParser<_, _, _>();
            ImplicitOpPrec = -1
            ImplicitOpMap = (fun x _ -> x)
        }

    let withImplicitOp prec map opBuilder = 
        if (prec <= 0) 
        then raise <| System.ArgumentOutOfRangeException "The operator precedence must be greater than 0."
        { opBuilder with ImplicitOpPrec = prec; ImplicitOpMap = map }

    let withPrefixOp  op prec map = addOp <| PrefixOperator  (op, stop, prec, false, map)

    let withPostfixOp op prec map = addOp <| PostfixOperator (op, stop, prec, false, map)

    let withInfixOp   op prec map = addOp <| InfixOperator   (op, stop, prec, Associativity.Left, map)

    let andWithTerms buildTermParser opBuilder = 
        opBuilder.High.TermParser <- buildTermParser opBuilder.Low.ExpressionParser
        opBuilder.Low.TermParser  <- many1 opBuilder.High.ExpressionParser |>> List.reduce opBuilder.ImplicitOpMap
        opBuilder.Low.ExpressionParser
