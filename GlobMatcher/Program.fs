open System
open GlobMatcher

let printGravizoLink states transitions =
    let fixDigitHead id =
        if (Char.IsDigit(id, 0)) then "_" + id else id
    let printState s =
        match s with
        | State (UniqueId id) -> fixDigitHead id
        | s -> sprintf "%A" s
    let printWord w =
        match w with
        | Word s -> if s = "" then "''" else s
        | w -> sprintf "%A" w
    let printTransition {Start = s; End = e; Accepts = w} =
        sprintf "%s->%s[label=\"%s\"]" (printState s) (printState e) (printWord w)
        
    let states' = states |> List.map printState |> String.concat ";"
    let transitions' = transitions |> List.map printTransition |> String.concat ";"
    let dotscript = "digraph G {" + states' + ";" + transitions' + "}"
    "https://g.gravizo.com/svg?" + (Uri.EscapeDataString dotscript) |> printfn "%s"

let isMatch pattern text printGraph =
    let startState::otherStates,transitions = Parser.toAcceptor pattern
    if printGraph then printGravizoLink (startState::otherStates) transitions
    Automaton.run startState transitions text

[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 1 -> isMatch argv.[0] "" false |> printfn "%A"
    | 2 -> isMatch argv.[0] argv.[1] false |> printfn "%A"
    | 3 when argv.[2] = "--printGraph" || argv.[2] = "-p" -> 
        isMatch argv.[0] argv.[1] true |> printfn "%A"
    | _ -> printfn "Usage: GlobMatcher.exe <pattern string> <test string> [--printGraph or -p]"

    0 // return an integer exit code
