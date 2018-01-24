namespace StringMatcher

module AutomatonPrinter =

    open System.Collections.Generic
    open StringMatcher

    type Transition = {Start: State; End: State; Accepts: Letter option}

    let private escape = function
        | '\\' -> "\\\\"
        | '"' -> "\\\""
        | c -> string c

    let private printLetter = function
        | Some (Letter c)         -> escape c
        | Some (Range (min, max)) -> sprintf "%s-%s" (escape min) (escape max)
        | Some (Any)              -> "*"
        | None                    -> ""

    let private printState = 
        let ids = Dictionary<State, int> ()
        let mutable id = -1
        fun state ->
            match ids.TryGetValue state with
            | true, id -> string id
            | false, _ -> 
                id <- id + 1
                ids.Add (state, id)
                string id

    let private printTransition {Start = s; End = e; Accepts = l} =
        sprintf "%s->%s[label=\"%s\"]" (printState s) (printState e) (printLetter l)

    let toDot start = 
        let visited = HashSet<State> ()

        let rec collectTransitions state =
            let alreadyDone = visited.Contains state
            match alreadyDone, state with
            | true, _ -> []
            | _, Final -> 
                visited.Add state |> ignore
                []
            | _, State (l, next) ->
                visited.Add state |> ignore
                let t = {Start = state; End = next; Accepts = Some l}
                t::collectTransitions next
            | _, Split (left, right) -> 
                visited.Add state |> ignore
                let t1 = {Start = state; End = left; Accepts = None}
                let t2 = {Start = state; End = right; Accepts = None}
                t1 :: t2 :: collectTransitions left @ collectTransitions right

        collectTransitions start
        |> List.map printTransition |> String.concat "; "
        |> sprintf "digraph G {%s}"
