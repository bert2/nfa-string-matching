open Acceptor
open Parser

let isMatch pattern text =
    let startState,transitions = toAcceptor pattern
    accept startState transitions text

[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 1 -> printfn "%A" (isMatch argv.[0] "")
    | 2 -> printfn "%A" (isMatch argv.[0] argv.[1])
    | _ -> printfn "Usage: GlobMatcher.exe <pattern string> <test string>"

    0 // return an integer exit code
