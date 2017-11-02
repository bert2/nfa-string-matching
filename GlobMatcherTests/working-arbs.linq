<Query Kind="FSharpProgram">
  <NuGetReference>FsCheck</NuGetReference>
</Query>

open System.Globalization
open FsCheck

let reverse s =
    seq {
        let e = StringInfo.GetTextElementEnumerator(s)
        while e.MoveNext() do
            yield e.GetTextElement()
    }
    |> Array.ofSeq
    |> Array.rev
    |> String.concat ""

type TestData = {Pattern: string; Text: string}

let stringFrom alphabet =
    alphabet |> Gen.elements |> Gen.listOf |> Gen.map (List.map string >> List.fold (+) "")

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
    let mutable text = ""

    for gen in Seq.map toGen pattern do
        let! textPart = gen
        text <- text + textPart

    return {Pattern = pattern; Text = text}
}

randomTextAndPatternCombo |> Gen.sample 5 10 |> Dump |> ignore
matchingTextAndPatternCombo |> Gen.sample 5 10 |> Dump |> ignore