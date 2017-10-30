module Parser

open System
open System.Globalization
open System.Text.RegularExpressions
open Acceptor

let private reverse s =
    seq {
        let e = StringInfo.GetTextElementEnumerator(s)
        while e.MoveNext() do
            yield e.GetTextElement()
    }
    |> Array.ofSeq
    |> Array.rev
    |> String.concat ""

let private newShortId () =
    let id = Guid.NewGuid().ToByteArray() |> Convert.ToBase64String
    Regex.Replace(id, "[/+=]", "")

let private parseWord (oldStart::states) transitions word =
    match word with
    | "*" -> oldStart::states, transitions
    | "?" ->
        let newStart = State (newShortId () |> Id)
        let t = {Start = newStart; End = oldStart; Accepts = Anything}
        newStart::oldStart::states, t::transitions
    | _ -> 
        let newStart = State (newShortId () |> Id)
        let t = {Start = newStart; End = oldStart; Accepts = Word word}
        newStart::oldStart::states, t::transitions
    
let rec private parse (start::states) transitions (pattern:string) =
    match pattern.Length with
    | 0 -> start, transitions
    | _ -> 
        let states', transistions' = parseWord (start::states) transitions pattern.[..0]
        parse states' transistions' pattern.[1..]

let toAcceptor pattern =
    let initial = State (newShortId () |> Id)
    let halt = Success
    let ``end of input`` = {Start = initial; End = halt; Accepts = Word ""}
    parse [initial; halt] [``end of input``] (reverse pattern)
