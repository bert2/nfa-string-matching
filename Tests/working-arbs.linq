<Query Kind="FSharpProgram">
  <NuGetReference>FsCheck</NuGetReference>
</Query>

open System.Globalization
open FsCheck

type TestData = {Pattern: string; Text: string}

let stringFrom alphabet =
    alphabet |> Gen.elements |> Gen.listOf |> Gen.map (List.map string >> String.concat "")

let singleCharStringFrom alphabet =
    alphabet |> Gen.elements |> Gen.map string

let randomTextAndPatternCombo = gen {
    let! pattern = stringFrom (['a'..'c']@['?'; '*'])
    let! text = stringFrom ['a'..'f']
    return {Pattern = pattern; Text = text}
}

let matchingTextAndPatternCombo = gen {    
    let toGen = function
        | '*' -> stringFrom ['a'..'f']
        | '?' -> singleCharStringFrom ['a'..'f']
        | c -> c |> string |> Gen.constant

    let! pattern = stringFrom (['a'..'c']@['?'; '*'])
    let! text = pattern |> Seq.map toGen |> Gen.sequence |> Gen.map (String.concat "")

    return {Pattern = pattern; Text = text}
}

randomTextAndPatternCombo |> Gen.sample 5 10 |> Dump |> ignore
matchingTextAndPatternCombo |> Gen.sample 5 10 |> Dump |> ignore