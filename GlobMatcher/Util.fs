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

let removeDuplicates xs =
    let mem = HashSet()
    [for x in xs do 
        if not (mem.Contains x) then
            mem.Add x |> ignore
            yield x]

let collectUnique selector = List.collect selector >> removeDuplicates

let intersects xs ys =
    xs |> List.exists (fun x -> ys |> List.contains x)