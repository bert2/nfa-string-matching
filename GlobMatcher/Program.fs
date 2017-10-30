open Acceptor

[<EntryPoint>]
let main argv = 
    // State machine for the pattern "a*c*e":
    let one = State
    let two = State
    let three = State
    let four = State
    let five = Success

    let a = {Start = one; End = two; Accepts = Word "a"}
    let ``* (1)`` = {Start = two; End = two; Accepts = Anything}
    let c = {Start = two; End = three; Accepts = Word "c"}
    let ``* (2)`` = {Start = three; End = three; Accepts = Anything}
    let e = {Start = three; End = four; Accepts = Word "e"}
    let ``e again`` = {Start = four; End = four; Accepts = Word "e"}
    let backtrack = {Start = four; End = three; Accepts = Anything}
    let ``end of string`` = {Start = four; End = five; Accepts = Word ""}

    let transistions = [a; ``* (1)``; c; ``* (2)``; e; ``e again``; backtrack; ``end of string``]

    let result = accept one transistions argv.[0]
    printfn "%A" result
    0 // return an integer exit code
