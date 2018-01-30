module GeneratedRegexPatterns

open Xunit
open FsCheck
open StringMatcher

type Regex = 
    | Char   of char
    | Concat of Regex * Regex
    | Alt    of Regex * Regex
    | Group  of Regex
    | Star   of Regex
    | Plus   of Regex
    | Opt    of Regex

let rec genRegex () = 
    let leaves = [
        Gen.map Char <| Gen.elements ['a'..'f']]

    let rec genRegex' size =
        if size <= 0 then
            Gen.oneof leaves
        else
            let subRegex = genRegex' (size - 1)
            let nested = [
                Gen.map Concat <| Gen.two subRegex
                Gen.map Alt    <| Gen.two subRegex
                Gen.map Group     subRegex
                Gen.map Star      subRegex
                Gen.map Plus      subRegex
                Gen.map Opt       subRegex]
            Gen.oneof <| leaves @ nested

    Gen.sized genRegex'

let rec printRegex = function
    | Char c        -> string c
    | Concat (l, r) -> sprintf "(%s)(%s)" (printRegex l) (printRegex r)
    | Alt (l, r)    -> sprintf "(%s)|(%s)" (printRegex l) (printRegex r)
    | Group x       -> sprintf "(%s)" (printRegex x)
    | Star x        -> sprintf "(%s)*"  (printRegex x)
    | Plus x        -> sprintf "(%s)+"  (printRegex x)
    | Opt x         -> sprintf "(%s)?"  (printRegex x)

let rec genText = function
    | Char c        -> Gen.constant <| string c
    | Concat (l, r) -> Gen.map2 (+) (genText l) (genText r)
    | Alt (l, r)    -> Gen.oneof [genText l; genText r]
    | Group r       -> genText r
    | Star r        -> genText r |> Gen.listOf |> Gen.map (List.map string >> String.concat "")
    | Plus r        -> genText r |> Gen.nonEmptyListOf |> Gen.map (List.map string >> String.concat "")
    | Opt r         -> genText r |> Gen.optionOf |> Gen.map (function | Some s -> s | None -> "")

let matchingTextAndPatternCombo = gen {
    let! regex = genRegex ()
    let pattern = printRegex regex
    let! text = genText regex
    return (pattern, text, regex)
}

[<Fact>]
let ``test`` () =
    Check.One ({Config.VerboseThrowOnFailure with EndSize = 30}, Prop.forAll 
        (Arb.fromGen matchingTextAndPatternCombo)
        (fun (pattern, text, _) -> 
            //System.IO.File.AppendAllText ("regextest.log", sprintf "pattern: %s\ntext:    %s\n%A\n\n" pattern text graph)
            System.IO.File.AppendAllText ("regextest.log", sprintf "pattern: %s\ntext:    %s\n\n\n" pattern text)
            let a = RegexParser.toAutomaton' pattern
            Autom.run a text))