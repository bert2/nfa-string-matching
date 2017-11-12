namespace GlobMatcher

type UniqueId = UniqueId of string
type State = State of UniqueId
type Word = Word of char | Epsilon
type Transition = {Start: State; End: State; Accepts: Word}
type Automaton = {Initial: State list; Final: State list; Transitions: Transition list}

module Automaton =
    open Util

    let private accepts word {Accepts = word'} = word' = word

    let private isOutgoingFrom state {Start = start} = state = start

    let private getReachable word transitions state =
        transitions
        |> List.filter (isOutgoingFrom state) 
        |> List.filter (accepts word) 
        |> List.map (fun {End = state'} -> state')

    let private consume word transitions =
        List.collect (getReachable word transitions) >> List.distinct

    let rec private addEpsilonReachable transitions added =
        let addEpsilonReachable' state =
            let added' = getReachable Epsilon transitions state |> List.except (state::added)
            match added' with 
            | [] -> [state]
            | _ -> state::addEpsilonReachable transitions (added@added') added'
        List.collect addEpsilonReachable' >> List.distinct

    let run {Initial = initial; Final = final; Transitions = transitions} text =
        let rec run' (text:string) current =
            let current' = addEpsilonReachable transitions [] current
            if text.Length = 0 then
                current' |> intersects final
            else
                let next = consume (Word text.[0]) transitions current'
                run' text.[1..] next
        run' text initial
