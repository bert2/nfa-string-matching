namespace GlobMatcher

module GlobParser = 

    open AutomatonBuilder
    open Parser
    open Result
    
    let private anyCharWildcard = pchar '?' |>> ignore

    let private anyStringWildcard = pchar '*' |>> ignore

    let private escapableChar = anyOf @"?*[]\"

    let private escapedChar = pchar '\\' >>. escapableChar <?> "escape sequence"

    let private char = satisfy (fun _ -> true) "char"

    let private charRange = pchar '[' >>. char .>> pchar '-' .>>. char .>> pchar ']' <?> "character range"

    let private parser =
        choice [
            anyCharWildcard   |>> makeAnyChar
            anyStringWildcard |>> makeAnyString
            charRange         |>> (fun (min, max) -> makeRange min max)
            escapedChar       |>> makeChar
            opt char          |>> (Option.map makeChar >> Option.defaultWith makeEmpty)
        ]

    let parsePattern succeed fail pattern =
        let result = run' parser pattern
        match result with
        | Failure _ -> fail <| toString result
        | Success (a, _) -> succeed a

    let toAutomaton = parsePattern Success Failure

    let toAutomaton' = parsePattern id failwith
