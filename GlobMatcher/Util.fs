module Util

open System
open System.Collections.Generic
open System.Globalization
open System.Text.RegularExpressions

let reverse s =
    seq {
        let e = StringInfo.GetTextElementEnumerator(s)
        while e.MoveNext() do
            yield e.GetTextElement()
    }
    |> Array.ofSeq
    |> Array.rev
    |> String.concat ""

let shortId () =
    let id = Guid.NewGuid().ToByteArray() |> Convert.ToBase64String
    Regex.Replace(id, "[/+=]", "")

let removeDuplicates source =
    let memory = HashSet()
    [for item in source do 
        if not (memory.Contains item) then
            memory.Add item |> ignore
            yield item]
