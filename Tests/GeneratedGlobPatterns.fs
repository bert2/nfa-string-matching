module GeneratedGlobPatterns

open Xunit
open FsCheck
open StringMatcher

type TestData = {Pattern: string; Text: string}

let stringFrom = Gen.elements >> Gen.listOf >> Gen.map (List.map string >> String.concat "")

let charToGen c =
    match c with
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
    Check.One ({Config.VerboseThrowOnFailure with MaxTest = 1000}, Prop.forAll 
        (Arb.fromGen matchingTextAndPatternCombo) 
        (fun {Pattern = pattern; Text = text} -> 
            let p = pattern
            let M = GlobParser.toAutomaton' p
            let result = Automaton.run M text
            result))
