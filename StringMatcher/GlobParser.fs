namespace StringMatcher

module GlobParser = 

    open FParsec
    
    let private anyCharWildcard = skipChar '?'

    let private anyStringWildcard = skipChar '*'

    let private escapableChar = 
        anyOf @"?*[]\" 
        <?> @"'?', '*', '[', ']', or '\'"

    let private escapedChar = skipChar '\\' >>. escapableChar 

    let private charRange = 
        skipChar '[' 
        >>.  anyChar 
        .>>  skipChar '-' 
        .>>. anyChar 
        .>>  skipChar ']' 
        <?>  "character range"

    let private token =
        choice [
            anyCharWildcard   |>> ProtoAutom.makeAnyChar
            anyStringWildcard |>> ProtoAutom.makeAnyChar |>> ProtoAutom.makeZeroOrMore
            charRange         |>> ProtoAutom.makeRange
            escapedChar       |>> ProtoAutom.makeChar
            anyChar           |>> ProtoAutom.makeChar
        ]

    let private parser = many token .>> eof |>> ProtoAutom.completeAll Final

    let parsePattern succeed fail pattern =
        let result = CharParsers.run parser pattern
        match result with
        | Failure (msg, _, _) -> fail msg
        | Success (a, _, _)   -> succeed a

    let toAutomaton = parsePattern Result.Success Result.Failure

    let toAutomaton' = parsePattern id failwith
