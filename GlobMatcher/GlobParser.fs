namespace GlobMatcher

module GlobParser = 

    open FParsec
    open AutomatonBuilder
    
    let private anyCharWildcard = skipChar '?'

    let private anyStringWildcard = skipChar '*'

    let x = anyStringWildcard

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
            anyCharWildcard   |>> makeAnyChar
            anyStringWildcard |>> makeAnyString
            charRange         |>> makeRange
            escapedChar       |>> makeChar
            anyChar           |>> makeChar
        ]

    let private parser = many token |>> List.foldBack' AutomatonBuilder.run empty

    let parsePattern succeed fail pattern =
        let result = CharParsers.run parser pattern
        match result with
        | Failure (msg, _, _) -> fail msg
        | Success (a, _, _)   -> succeed a

    let toAutomaton = parsePattern Result.Success Result.Failure

    let toAutomaton' = parsePattern id failwith
