namespace StringMatcher

module ArgsParser =

    open System
    open FParsec
    open Util

    type Arg = ArgValue of string * string | ArgSwitch of string

    // helpers

    let private isWordChar c = Char.IsLetterOrDigit c || c = '-' || c = '_'

    let private letter = satisfy Char.IsLetter

    let private wordchar = satisfy isWordChar

    let private noneSpace = satisfy (not << Char.IsWhiteSpace)

    let private noneQuote = satisfy (not << (=) '\'')

    let private noneDoubleQuote = satisfy (not << (=) '"')

    let private noneDash = satisfy (not << (=) '-')

    let private identifier = letter .>>. many1 wordchar |>> (List.Cons >> charsToStr)

    // argument keys

    let private longArgkey = skipString "--" >>. identifier <?> "--argumentKey"

    let private shortArgkey = skipChar '-' >>. letter |>> string <?> "argument shorthand (-a)"

    let private argkey = longArgkey <|> shortArgkey

    // argument values

    let private unquotedArgvalue = many1Chars2 noneDash noneSpace

    let private quotedArgvalue = skipChar '\'' >>. manyChars noneQuote .>> skipChar '\''

    let private doubleQuotedArgvalue = skipChar '"' >>. manyChars noneDoubleQuote .>> skipChar '"'

    let private argvalue = quotedArgvalue <|> doubleQuotedArgvalue <|> unquotedArgvalue

    // argument list

    let private arg = argkey .>> spaces1 .>>. argvalue .>> spaces

    let private switch = argkey .>> spaces |>> fun k -> (k, "")

    let private parser = many (attempt arg <|> switch) .>> eof

    let parseArgs argv = 
        let result = String.concat " " argv |> run parser
        match result with
        | Failure (msg, _, _)  -> Result.Failure msg
        | Success (args, _, _) -> Result.Success <| Map.fromPairs args
