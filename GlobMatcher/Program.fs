open Acceptor
open Parser

[<EntryPoint>]
let main argv = 
    // State machine for the pattern "a*c*e":
    let start = State (UniqueId "start")
    let one = State (UniqueId "one")
    let two = State (UniqueId "two")
    let three = State (UniqueId "three")
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
    testParser "??" "ab"
    testParser "??" "a"
    testParser "a??d" "abcd"
    testParser "a??d" "abcde"
    testParser "*" ""
    testParser "*" "a"
    testParser "*" "abc"
    testParser "a*" ""
    testParser "a*" "a"
    testParser "a*" "abc"
    testParser "a*" "xyz"
    testParser "*b" "ab"
    testParser "*b" "abb"
    testParser "*b" "abab"
    testParser "*b" "ababa"
    testParser "a*c" "ac"
    testParser "a*c" "acc"
    testParser "a*c" "abc"
    testParser "a*c" "abcabc"
    testParser "a*c" "abcdabc"
    testParser "a*c*e" "ace"
    testParser "a*c*e" "abcde"
    testParser "a*c*e" "abcdef"
    testParser "a*c*e" "abcbcde"
    testParser "a*c*e" "abcbcde"
    testParser "a*c*e" "abccde"
    testParser "a*c*e" "abcdede"
    testParser "a*c*e" "abcdee"
    testParser "a*c*ef" "abcdef"
    testParser "a*c*ef" "abcdefx"
    testParser "a*c*ef" "abcdeef"
    testParser "a*c*ef" "abcdeefef"

    0 // return an integer exit code
