namespace GlobMatcher

module AutomatonBuilder =
    
    open Util

    type ProtoAutomaton = ProtoAutomaton of (State -> State)

    let complete (ProtoAutomaton completeWith) exit = completeWith exit

    let combine first second = ProtoAutomaton (complete first << complete second)

    let zero = ProtoAutomaton id

    let private newId = 
        let mutable i = -1
        fun () ->
            i <- i + 1
            i  |> string |> Id

    let private accepting word exit = State (newId (), word, exit)

    let private loop body exit =
        let rec split = Split (newId (), enter, exit)
        and enter = complete body split
        split

    let private option inner skip =
        let s1 = complete inner skip
        Split (newId (), s1, skip)

    let makeChar = ProtoAutomaton << accepting << Word

    let makeAnyChar () = ProtoAutomaton << accepting <| Any

    let makeRange = ProtoAutomaton << accepting << Range << sortTuple

    let makeZeroOrMore = ProtoAutomaton << loop

    let makeOneOrMore inner = ProtoAutomaton (complete inner << loop inner)

    let makeZeroOrOne = ProtoAutomaton << option   
