namespace StringMatcher

type Id = Id of string

type Letter = 
    | Letter of char
    | Range of char * char 
    | Any

type State = 
    | State of Id * Letter * State
    | Split of Id * State * State
    | Final

module Automaton =

    let getId state =
        match state with
        | State (id, _, _) -> id
        | Split (id, _, _) -> id
        | Final            -> Id "Final"

    let private step letter state = Util.tail2 [state] [] (fun s ->
        match s, letter with
        | Split (_, l, r)                  , _        -> Some (l, r), None
        | State (_, Any, next)             , _        -> None, Some next
        | State (_, Letter c', next)       , Letter c          
            when c' = c                               -> None, Some next
        | State (_, Range (min, max), next), Letter c 
            when min <= c && c <= max                 -> None, Some next
        | _                                           -> None, None)

    let private expandEpsilons state = Util.tail2 [state] [] (fun s ->
        match s with
        | Split (_, l, r) -> Some (l, r), None
        | _               -> None, Some s)
    
    let private consume currents letter =
       currents 
       |> List.collect (step letter) 
       |> List.collect expandEpsilons
       |> List.distinctBy getId

    let run start = 
        Seq.map Letter 
        >> Seq.fold consume (expandEpsilons start)
        >> List.contains Final
