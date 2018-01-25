﻿namespace StringMatcher

module AutomPrinter =

    open System.Collections.Generic
    open StringMatcher
    open Util

    type Transition = {Start: State; End: State; Accepts: Letter option}

    let private escape = function
        | '\\' -> "\\\\"
        | '"'  -> "\\\""
        | c    -> string c

    let private printLetter = function
        | Some (Letter c)         -> escape c
        | Some (Range (min, max)) -> sprintf "%s-%s" (escape min) (escape max)
        | Some (Any)              -> "*"
        | None                    -> ""

    let private printState = 
        let nextId =
            let mutable id = -1
            fun () -> id <- id + 1; id
        memoize (function 
            | Final -> "F" 
            | _     -> nextId () |> string)

    let private printTransition {Start = s; End = e; Accepts = l} =
        sprintf "%s->%s[label=\"%s\"]" (printState s) (printState e) (printLetter l)

    let private collectTransitions state =
        let visited = HashSet<_> ()

        let rec collectTransitions' state =
            if visited.Contains state then 
                [] 
            else 
                match state with
                | Final -> []
                | State (l, next) ->
                    visited.Add state |> ignore
                    let t = {Start = state; End = next; Accepts = Some l}
                    t::collectTransitions' next
                | Split (left, right) -> 
                    visited.Add state |> ignore
                    let t1 = {Start = state; End = left; Accepts = None}
                    let t2 = {Start = state; End = right; Accepts = None}
                    t1 :: t2 :: collectTransitions' left @ collectTransitions' right

        collectTransitions' state

    let toDot = 
        collectTransitions
        >> List.map printTransition 
        >> String.concat "; "
        >> sprintf "digraph G {%s}"
