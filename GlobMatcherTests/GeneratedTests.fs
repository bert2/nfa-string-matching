module GeneratedTests

open System.Text.RegularExpressions
open Xunit
open FsCheck
open GlobMatcher

type TestData = {Pattern: string; Text: string}

let stringFrom alphabet =
    alphabet
    |> Gen.elements 
    |> Gen.nonEmptyListOf 
    |> Gen.map (List.map string >> List.fold (+) "")
let patterns = stringFrom (['a'..'c']@['?'; '*'])
let texts = stringFrom (['a'..'f'])
let gen = gen {
    let! pattern = stringFrom (['a'..'c']@['?'; '*'])
    let! text = stringFrom (['a'..'f'])
    return {Pattern = pattern; Text = text}
}

let makeLabel globResult regexResult = 
    sprintf 
        "text did%s match glob but did%s match regex" 
        (if globResult then "" else " NOT") 
        (if regexResult then "" else " NOT") 

[<Fact>]
let ``equivalent regular expression yields then same match result`` () = 
    Check.VerboseThrowOnFailure (Prop.forAll 
        (Arb.fromGen gen) 
        (fun {Pattern = pattern; Text = text} -> 
            let startState::_, transitions = Parser.toAcceptor pattern
            let result = Acceptor.run startState transitions text

            let pattern' = "^" + pattern.Replace("*", ".*").Replace("?", ".") + "$"
            let result' = Regex.IsMatch(text, pattern')

            result = result' |@ makeLabel result result'))