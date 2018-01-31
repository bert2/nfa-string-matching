module Util

open System.Collections.Generic

let sortTuple (l, r) = (min l r, max l r)

let memoize f =
    let cache = Dictionary<_,_> ()
    fun x ->
        match cache.TryGetValue x with
        | true, y -> y
        | false, _ ->
            let y = f x
            cache.Add (x, y)
            y

let visitEach defaultValue f x =
    let visited = HashSet<_> ()
    let rec visit x = 
        if visited.Contains x then 
            defaultValue 
        else 
            visited.Add x |> ignore
            f visit x
    visit x