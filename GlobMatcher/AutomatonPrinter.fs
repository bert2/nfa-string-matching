namespace GlobMatcher

module AutomatonPrinter =

    open System
    open System.Collections.Generic
    open GlobMatcher
    open Automaton

    type Transition = {Start: Id; End: Id; Accepts: Word option}

    let private prefixDigitHead id = 
        let headIsDigit = Char.IsDigit(id, 0)
        let onlyDigits = id |> Seq.forall Char.IsDigit
        if headIsDigit && not onlyDigits then "_" + id else id

    let private escape c = 
        match c with
        | '\\' -> "\\\\"
        | '"' -> "\\\""
        | c -> string c

    let private printStateId (Id id) = prefixDigitHead id

    let private printWord w =
        match w with
        | Some (Word c)           -> escape c
        | Some (Range (min, max)) -> sprintf "%s-%s" (escape min) (escape max)
        | Some (Any)              -> "*"
        | None                    -> ""

    let private printTransition {Start = s; End = e; Accepts = w} =
        sprintf "%s->%s[label=\"%s\"]" (printStateId s) (printStateId e) (printWord w)

    let toDot start = 
        let visited = HashSet<Id> ()

        let rec collectTransitions state =
            let alreadyDone = visited.Contains (getId state)
            match alreadyDone, state with
            | true, _ -> []
            | _, Final -> 
                getId state |> visited.Add |> ignore
                []
            | _, State (id, w, next) ->
                visited.Add id |> ignore
                let t = {Start = id; End = getId next; Accepts = Some w}
                t::collectTransitions next
            | _, Split (id, left, right) -> 
                visited.Add id |> ignore
                let t1 = {Start = id; End = getId left; Accepts = None}
                let t2 = {Start = id; End = getId right; Accepts = None}
                t1 :: t2 :: collectTransitions left @ collectTransitions right

        collectTransitions start
        |> List.map printTransition |> String.concat "; "
        |> sprintf "digraph G {%s}"
