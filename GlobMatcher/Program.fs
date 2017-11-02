open Acceptor
open Parser

let isMatch pattern text printGraph =
    let startState::otherStates,transitions = toAcceptor pattern
    if printGraph then printfn "%s" (printGravizoLink (startState::otherStates) transitions)
    accept startState transitions text

[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 1 -> isMatch argv.[0] "" false |> printfn "%A"
    | 2 -> isMatch argv.[0] argv.[1] false |> printfn "%A"
    | 3 when argv.[2] = "--printGraph" || argv.[2] = "-p" -> 
        isMatch argv.[0] argv.[1] true |> printfn "%A"
    | _ -> printfn "Usage: GlobMatcher.exe <pattern string> <test string> [--printGraph or -p]"

    0 // return an integer exit code
