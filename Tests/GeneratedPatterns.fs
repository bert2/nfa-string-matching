module GeneratedPatterns

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

let matchingTextAndPatternCombo = gen {
    let! pattern = stringFrom (['a'..'c']@['?'; '*'])
    let! text = pattern |> Seq.map charToGen |> Gen.sequence |> Gen.map (String.concat "")
    return {Pattern = pattern; Text = text}
}

[<Fact>]
let ``matching pattern and text are accepted`` () = 
    Check.VerboseThrowOnFailure (Prop.forAll 
        (Arb.fromGen matchingTextAndPatternCombo) 
        (fun {Pattern = pattern; Text = text} -> 
            let M = GlobParser.toAutomaton pattern
            let result = Automaton.run M text
            result))
