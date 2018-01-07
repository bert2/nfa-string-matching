module Util

let sortTuple (l, r) = (min l r, max l r)

let globalCount = 
    let mutable i = -1
    fun () ->
        i <- i + 1
        i

let charsToStr = List.toArray >> System.String >> string

module Map =

    let fromPairs pairs = 
        let add m (k, v) = Map.add k v m
        Seq.fold add Map.empty pairs

module List =

    let inline foldBack' folder state list = List.foldBack folder list state

    let inline front list = list |> List.truncate (list.Length - 1)

