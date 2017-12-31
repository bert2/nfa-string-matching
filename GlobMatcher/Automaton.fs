namespace GlobMatcher

type Id = Id of string
type Letter = Letter of char | Range of char * char | Any
type State = State of Id * Letter * State | Split of Id * State * State | Final

module Automaton =

    let getId state =
        match state with
        | State (id, _, _) -> id
        | Split (id, _, _) -> id
        | Final            -> Id "Final"
    
    let rec private step letter state =
        match letter, state with
        | l     , Split (_, left, right)            -> (step l left)@(step l right)
        | _     , State (_, Any, next)              -> [next]
        | Letter c, State (_, Letter c', next)          
            when c = c'                             -> [next]
        | Letter c, State (_, Range (min, max), next) 
            when min <= c && c <= max               -> [next]
        | _                                         -> []

    let rec private expandEpsilons state =
        match state with
        | Split (_, left, right) -> (expandEpsilons left)@(expandEpsilons right)
        | state -> [state]
    
    let private consume currents letter =
       currents 
       |> List.collect (step letter) 
       |> List.collect expandEpsilons
       |> List.distinctBy getId

    let run start = 
        Seq.map Letter 
        >> Seq.fold consume (expandEpsilons start)
        >> List.contains Final
