module GeneratedTests

open Xunit
open FsCheck
open Parser
open Acceptor

let stringFrom alphabet =
    alphabet
    |> Gen.elements 
    |> Gen.nonEmptyListOf 
    |> Gen.map (List.map string >> List.fold (+) "")
    |> Arb.fromGen
let patterns = stringFrom (['a'..'c']@['?'; '*'])
let texts = stringFrom (['a'..'f'])

type Pattern = Pattern of string with
    static member op_Explicit(Pattern s) = s

type Text = Text of string with
    static member op_Explicit(Text s) = s

type MyArbitraries =
    static member Pattern() = 
        stringFrom (['a'..'c']@['?'; '*'])
        |> Arb.convert Pattern string
    static member Text() = 
        stringFrom (['a'..'f'])
        |> Arb.convert Text string

Arb.register<MyArbitraries>() |> ignore

//[<Property>]
//let ``test`` (Pattern p) (Text t) =
//    p.Length = t.Length

[<Fact>]
let test () = 
    let prop (Pattern p) (Text t) = p.Length = t.Length
    Check.QuickThrowOnFailure prop
