module GeneratedGlobPatterns

open Xunit
open FsCheck
open StringMatcher

type TestData = {Pattern: string; Text: string}

let stringFrom = Gen.elements >> Gen.listOf >> Gen.map (List.map string >> String.concat "")

let charToGen = function
    | '*' -> stringFrom ['a'..'f']
    | '?' -> ['a'..'f'] |> Gen.elements |> Gen.map string
    | c   -> c |> string |> Gen.constant

let matchingTextAndPatternCombo = gen {
    let! pattern = stringFrom (['a'..'c']@['?'; '*'])
    let! text = pattern |> Seq.map charToGen |> Gen.sequence |> Gen.map (String.concat "")
    return {Pattern = pattern; Text = text}
}

[<Fact>]
let ``matching pattern and text are accepted`` () = 
    Check.One ({Config.VerboseThrowOnFailure with MaxTest = 3000}, Prop.forAll 
        (Arb.fromGen matchingTextAndPatternCombo) 
        (fun {Pattern = pattern; Text = text} -> 
            let a = GlobParser.toAutomaton' pattern
            Automaton.run a text))
