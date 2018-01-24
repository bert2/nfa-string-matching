module Util

let sortTuple (l, r) = (min l r, max l r)

module List =

    let inline foldBack' folder state list = List.foldBack folder list state
