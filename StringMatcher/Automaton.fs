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
    
    let private step letter state =
        // tail-recursive helper function
        let rec step' todo stepped =
            match todo with
            | []         -> stepped
            | s::todo' ->
                match s, letter with
                | Split (_, l, r)                  , _        -> step' (l::r::todo') stepped
                | State (_, Any, next)             , _        -> step' todo' (next::stepped)
                | State (_, Letter c', next)       , Letter c          
                    when c' = c                               -> step' todo' (next::stepped)
                | State (_, Range (min, max), next), Letter c 
                    when min <= c && c <= max                 -> step' todo' (next::stepped)
                | _                                           -> step' todo' stepped
        step' [state] []

    let private expandEpsilons state =
        // tail-recursive helper function
        let rec expand todo expanded =
            match todo with
            | []           -> expanded
            | s::todo' ->
                match s with
                | Split (_, l, r) -> expand (l::r::todo') expanded
                | _               -> expand todo' (s::expanded)
        expand [state] []
    
    let private consume currents letter =
       currents 
       |> List.collect (step letter) 
       |> List.collect expandEpsilons
       |> List.distinctBy getId

    let run start = 
        Seq.map Letter 
        >> Seq.fold consume (expandEpsilons start)
        >> List.contains Final
