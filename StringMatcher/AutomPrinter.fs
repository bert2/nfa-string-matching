namespace StringMatcher

module AutomPrinter =

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

    let private collectTransitions = visitEach [] (fun visitNext state -> 
        match state with
        | Final -> []
        | State (l, next) ->
            let t = {Start = state; End = next; Accepts = Some l}
            t :: visitNext next
        | Split (left, right) -> 
            let t1 = {Start = state; End = left; Accepts = None}
            let t2 = {Start = state; End = right; Accepts = None}
            t1 :: t2 :: visitNext left @ visitNext right)

    let toDot = 
        collectTransitions
        >> List.map printTransition 
        >> String.concat "; "
        >> sprintf "digraph G {%s}"
