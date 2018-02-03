namespace StringMatcher

module Autom =

    let private moveNext letter state =
        match state, letter with
        | State (Any, next)             , _        -> Some next
        | State (Letter c', next)       , Letter c          
            when c' = c                            -> Some next
        | State (Range (min, max), next), Letter c 
            when min <= c && c <= max              -> Some next
        | _                                        -> None
    
    // Given a list of states expandAll adds all states to the list that are
    // reachable via transitions that don't consume intput.
    let private expandAll =
        let rec expandAll' visited expanded toExpand = 
            match toExpand with
            | [] -> expanded
            | state::toExpand' ->
                if visited |> Set.contains state then
                    // Breaks transition loops.
                    expandAll' visited expanded toExpand'
                else
                    match state with
                    | Split (l, r) -> 
                        let visited' = visited |> Set.add state
                        expandAll' visited' expanded (l::r::toExpand')
                    | _ -> 
                        let expanded' = expanded |> Set.add state
                        expandAll' visited expanded' toExpand'
        expandAll' Set.empty Set.empty

    let private expand = List.singleton >> expandAll

    let private consume currents letter =
       currents 
       |> Seq.choose (moveNext letter) 
       |> Seq.toList
       |> expandAll

    let run start = 
        Seq.map Letter 
        >> Seq.fold consume (expand start)
        >> Set.contains Final
