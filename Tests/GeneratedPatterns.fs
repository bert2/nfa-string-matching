module GeneratedPatterns

open System.Text.RegularExpressions
open Xunit
open FsCheck
open GlobMatcher

type TestData = {Pattern: string; Text: string}

let stringFrom alphabet =
    alphabet |> Gen.elements |> Gen.listOf |> Gen.map (List.map string >> String.concat "")

let singleCharStringFrom alphabet =
    alphabet |> Gen.elements |> Gen.map string

let charToGen c =
    match c with
    | '*' -> stringFrom ['a'..'f']
    | '?' -> singleCharStringFrom ['a'..'f']
    | c -> singleCharStringFrom [c]

let randomTextAndPatternCombo = gen {
    let! pattern = stringFrom (['a'..'c']@['?'; '*'])
    let! text = stringFrom ['a'..'f']
    return {Pattern = pattern; Text = text}
}

let matchingTextAndPatternCombo = gen {
    let! pattern = stringFrom (['a'..'c']@['?'; '*'])
    let! text = pattern |> Seq.map charToGen |> Gen.sequence |> Gen.map (String.concat "")
    return {Pattern = pattern; Text = text}
}

[<Fact>]
let ``equivalent regular expression yields then same match result`` () = 
    let print globResult regexResult = 
        sprintf 
            "text did%s match glob but did%s match regex" 
            (if globResult then "" else " NOT") 
            (if regexResult then "" else " NOT") 

    Check.VerboseThrowOnFailure (Prop.forAll 
        (Arb.fromGen randomTextAndPatternCombo) 
        (fun {Pattern = pattern; Text = text} -> 
            let startState::_, transitions = Parser.toAcceptor pattern
            let result = Automaton.run startState transitions text

            let pattern' = "^" + pattern.Replace("*", ".*").Replace("?", ".") + "$"
            let result' = Regex.IsMatch(text, pattern')

            result = result' |@ print result result'))

[<Fact>]
let ``matching pattern and text are accepted`` () = 
    Check.VerboseThrowOnFailure (Prop.forAll 
        (Arb.fromGen matchingTextAndPatternCombo) 
        (fun {Pattern = pattern; Text = text} -> 
            let startState::_, transitions = Parser.toAcceptor pattern
            let result = Automaton.run startState transitions text
            result))
