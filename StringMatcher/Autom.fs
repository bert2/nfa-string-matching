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
    open System.Collections.Generic

    let rec private moveNext letter state =
        match state, letter with
        | State (Any, next)             , _        -> [next]
        | State (Letter c', next)       , Letter c          
            when c' = c                            -> [next]
        | State (Range (min, max), next), Letter c 
            when min <= c && c <= max              -> [next]
        | _                                        -> []

    let private expandEpsilons state =
        let visited = HashSet<_> ()
        let rec expandEpsilons' state = 
            if visited.Contains state then 
                [] 
            else 
                match state with
                | Split (left, right) -> 
                    visited.Add state |> ignore
                    expandEpsilons' left @ expandEpsilons' right
                | state -> [state]
        expandEpsilons' state
    
    let private consume currents letter =
       currents 
       |> List.collect (moveNext letter) 
       |> List.collect expandEpsilons
       |> List.distinct

    let run start = 
        Seq.map Letter 
        >> Seq.fold consume (expandEpsilons start)
        >> List.contains Final
