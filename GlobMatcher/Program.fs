open Acceptor
open Parser

let isMatch pattern text printGraph =
    let startState::otherStates,transitions = toAcceptor pattern
    if printGraph then printfn "%s" (printGravizoLink (startState::otherStates) transitions)
    accept startState transitions text

[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 1 -> 
        printfn "%A" (isMatch argv.[0] "" false)
    | 2 -> 
        printfn "%A" (isMatch argv.[0] argv.[1] false)
    | 3 when argv.[2] = "--printGraph" || argv.[2] = "-p" -> 
        printfn "%A" (isMatch argv.[0] argv.[1] true)
    | _ -> 
        printfn "Usage: GlobMatcher.exe <pattern string> <test string> [--printGraph or -p]"

    0 // return an integer exit code
