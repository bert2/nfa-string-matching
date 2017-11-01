module GeneratedTests

open Xunit
open Xunit.Abstractions
open FsCheck
open Parser
open Acceptor
open FsCheck.Prop
open System.Text.RegularExpressions

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

type MyTests (output:ITestOutputHelper) =
    [<Fact>]
    member __.``equivalent regular expression yields then same match result`` () = 
        Check.VerboseThrowOnFailure (Prop.forAll 
            (Arb.fromGen gen) 
            (fun {Pattern = pattern; Text = text} -> 
                let startState, transitions = toAcceptor pattern
                let result = accept startState transitions text
                output.WriteLine("'{0}' matches glob '{1}': {2}", text, pattern, result)

                let pattern' = "^" + pattern.Replace("*", ".*").Replace("?", ".") + "$"
                let result' = Regex.IsMatch(text, pattern')
                output.WriteLine("'{0}' matches regex '{1}': {2}", text, pattern', result')

                result' = result))