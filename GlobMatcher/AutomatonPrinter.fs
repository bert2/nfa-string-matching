namespace GlobMatcher

module AutomatonPrinter =

    open System
    open GlobMatcher

    let private prefixDigitHead id = if (Char.IsDigit(id, 0)) then "_" + id else id

    let private printState (State (UniqueId id)) = prefixDigitHead id

    let private printWord w =
        match w with
        | Word c -> string c
        | Range (min, max) -> sprintf "%c-%c" min max
        | Any -> "*"
        | Epsilon -> ""

    let private printTransition {Start = s; End = e; Accepts = w} =
        sprintf "%s->%s[label=\"%s\"]" (printState s) (printState e) (printWord w)

    let toDot {Transitions = transitions} =        
        let transitions' = transitions |> List.map printTransition |> String.concat ";"
        "digraph G {" + transitions' + "}"
