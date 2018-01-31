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
        | State (Any, next)             , _        -> Some next
        | State (Letter c', next)       , Letter c          
            when c' = c                            -> Some next
        | State (Range (min, max), next), Letter c 
            when min <= c && c <= max              -> Some next
        | _                                        -> None

    let private skipEpsilons state =
        let visited = HashSet<_> ()
        let rec skipEpsilons' state = 
            if visited.Contains state then 
                [] 
            else 
                match state with
                | Split (left, right) -> 
                    visited.Add state |> ignore
                    skipEpsilons' left @ skipEpsilons' right
                | state -> [state]
        skipEpsilons' state
    
    let private consume currents letter =
       currents 
       |> List.choose (moveNext letter) 
       |> List.collect skipEpsilons
       |> List.distinct

    let run start = 
        Seq.map Letter 
        >> Seq.fold consume (skipEpsilons start)
        >> List.contains Final
