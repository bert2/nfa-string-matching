module Util

open System
open System.Text.RegularExpressions

let shortId () =
    let id = Guid.NewGuid().ToByteArray() |> Convert.ToBase64String
    Regex.Replace(id, "[/+=]", "")

let intersects xs ys =
    xs |> List.exists (fun x -> ys |> List.contains x)
