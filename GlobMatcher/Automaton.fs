namespace GlobMatcher

type Id = Id of string
type Word = Word of char | Range of char * char | Any
type State = State of Id * Word * State | Split of Id * State * State | Final

module Automaton =

    let getId state =
        match state with
        | State (id, _, _) -> id
        | Split (id, _, _) -> id
        | Final            -> Id "Final"
    
    let rec private step word state =
        match word, state with
        | w     , Split (_, left, right)            -> (step w left)@(step w right)
        | _     , State (_, Any, next)              -> [next]
        | Word c, State (_, Word c', next)          
            when c = c'                             -> [next]
        | Word c, State (_, Range (min, max), next) 
            when min <= c && c <= max               -> [next]
        | _                                         -> []

    let rec private expandEpsilons state =
        match state with
        | Split (_, left, right) -> (expandEpsilons left)@(expandEpsilons right)
        | state -> [state]
    
    let private consume word =
       List.collect (step word) >> List.collect expandEpsilons >> List.distinctBy getId

    let run start text =
        let rec run' (text:string) current =
            if text.Length = 0 then
                current |> List.contains Final
            else
                let next = consume (Word text.[0]) current
                run' text.[1..] next
        expandEpsilons start |> run' text
