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

module List =

    let inline foldBack' folder state list = List.foldBack folder list state
