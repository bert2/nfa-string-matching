open Acceptor
open System

[<EntryPoint>]
let main argv = 
    // State machine for the pattern "a*c*e":
    let one = State (Id "one")
    let two = State (Id "two")
    let three = State (Id "three")
    let four = State (Id "four")
    let five = Success

    let a = {Start = one; End = two; Accepts = Word "a"}
    let ``* (1)`` = {Start = two; End = two; Accepts = Anything}
    let c = {Start = two; End = three; Accepts = Word "c"}
    let ``* (2)`` = {Start = three; End = three; Accepts = Anything}
    let e = {Start = three; End = four; Accepts = Word "e"}
    let ``e again`` = {Start = four; End = four; Accepts = Word "e"}
    let backtrack = {Start = four; End = three; Accepts = Anything}
    let ``end of string`` = {Start = four; End = five; Accepts = Word ""}

    let transitions = [a; ``* (1)``; c; ``* (2)``; e; ``e again``; backtrack; ``end of string``]
    let text =
        match argv.Length with
        | 0 -> "acef"
        | _ -> argv.[0]

    let result = accept one transitions text
    printfn "%A" result
    0 // return an integer exit code
