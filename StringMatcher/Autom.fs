namespace StringMatcher

type Letter = 
    | Letter of char
    | Range of char * char 
    | Any

[<ReferenceEquality>]
type State = 
    | State of Letter * State
    | Split of State * State
    | Final

module Autom =

    let rec private step letter state =
        match state, letter with
        | Split (left, right)           , l        -> step l left @ step l right
        | State (Any, next)             , _        -> [next]
        | State (Letter c', next)       , Letter c          
            when c' = c                            -> [next]
        | State (Range (min, max), next), Letter c 
            when min <= c && c <= max              -> [next]
        | _                                        -> []

    let rec private expandEpsilons = function
        | Split (left, right) -> expandEpsilons left @ expandEpsilons right
        | state -> [state]
    
    let private consume currents letter =
       currents 
       |> List.collect (step letter) 
       |> List.collect expandEpsilons
       |> List.distinct

    let run start = 
        Seq.map Letter 
        >> Seq.fold consume (expandEpsilons start)
        >> List.contains Final
