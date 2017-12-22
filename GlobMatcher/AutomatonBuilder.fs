namespace GlobMatcher

module AutomatonBuilder =
    
    open Util

    type ProtoAutomaton = ProtoAutomaton of (State -> State)

    let run (ProtoAutomaton finish) next = finish next

    let combine proto1 proto2 = ProtoAutomaton (run proto2 >> run proto1)

    let zero = ProtoAutomaton id

    let private newId = 
        let mutable i = -1
        fun () ->
            i <- i + 1
            i  |> string |> Id

    let private accepting word toState = State (newId (), word, toState)

    let empty = Final

    let makeChar = ProtoAutomaton << accepting << Word

    let makeAnyChar () = ProtoAutomaton << accepting <| Any

    let makeRange = ProtoAutomaton << accepting << Range << sortTuple

    let makeZeroOrMore inner =
        ProtoAutomaton (fun next ->
            let rec s0 = Split (newId (), s1, next)
            and s1 = run inner s0
            s0)

    let makeOneOrMore inner =
        ProtoAutomaton (fun next ->
            let rec s1 = Split (newId (), s0, next)
            and s0 = run inner s1
            s0)

    let makeZeroOrOne inner =
        ProtoAutomaton (fun next ->
            let s1 = run inner next
            let s0 = Split (newId (), s1, next)
            s0)   
