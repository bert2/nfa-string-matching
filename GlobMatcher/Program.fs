open System
open GlobMatcher

let printGravizoLink {Transitions = transitions} =
    let fixDigitHead id = if (Char.IsDigit(id, 0)) then "_" + id else id
    let printState (State (UniqueId id)) = fixDigitHead id
    let printWord w =
        match w with
        | Word c -> string c
        | Range (min, max) -> sprintf "%c-%c" min max
        | Any -> "*"
        | Epsilon -> ""
    let printTransition {Start = s; End = e; Accepts = w} =
        sprintf "%s->%s[label=\"%s\"]" (printState s) (printState e) (printWord w)
        
    let transitions' = transitions |> List.map printTransition |> String.concat ";"
    let dotscript = "digraph G {" + transitions' + "}"
    "https://g.gravizo.com/svg?" + (Uri.EscapeDataString dotscript) |> printfn "%s"

let isMatch pattern text printGraph =
    let M = GlobParser.toAutomaton pattern
    if printGraph then printGravizoLink M
    Automaton.run M text

[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 1 -> isMatch argv.[0] "" false |> printfn "%A"
    | 2 -> isMatch argv.[0] argv.[1] false |> printfn "%A"
    | 3 when argv.[2] = "--printGraph" || argv.[2] = "-p" -> 
        isMatch argv.[0] argv.[1] true |> printfn "%A"
    | _ -> printfn "Usage: GlobMatcher.exe <pattern string> <test string> [--printGraph or -p]"

    0 // return an integer exit code
