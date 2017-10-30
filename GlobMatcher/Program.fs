open Acceptor
open Parser

[<EntryPoint>]
let main argv = 
    // State machine for the pattern "a*c*e":
    let start = State (Id "start")
    let one = State (Id "one")
    let two = State (Id "two")
    let three = State (Id "three")
    let stop = Success

    let a = {Start = start; End = one; Accepts = Word "a"}
    let ``* (1)`` = {Start = one; End = one; Accepts = Anything}
    let c = {Start = one; End = two; Accepts = Word "c"}
    let ``* (2)`` = {Start = two; End = two; Accepts = Anything}
    let e = {Start = two; End = three; Accepts = Word "e"}
    let ``e again`` = {Start = three; End = three; Accepts = Word "e"}
    let backtrack = {Start = three; End = two; Accepts = Anything}
    let ``end of string`` = {Start = three; End = stop; Accepts = Word ""}

    let transitions = [a; ``* (1)``; c; ``* (2)``; e; ``e again``; backtrack; ``end of string``]
    let text =
        match argv.Length with
        | 0 -> "abcde"
        | _ -> argv.[0]

    let result = accept start transitions text
    printfn "%A" result

    let testParser pattern text =
        let s,ts = toAcceptor pattern
        printfn "pattern: %A,\ttext: %A\t-> %A" pattern text (accept s ts text)
    
    testParser "" ""
    testParser "" "a"
    testParser "a" "a"
    testParser "a" "b"
    testParser "a" ""
    testParser "ab" "ab"
    testParser "ab" "abc"
    testParser "?" ""
    testParser "?" "a"
    testParser "?" "b"
    testParser "?" "ab"

    0 // return an integer exit code
