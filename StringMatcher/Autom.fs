namespace StringMatcher

type Letter = Any | Letter of char | Range of char * char 

[<CustomEquality; CustomComparison>]
type State = 
    | State of Letter * State
    | Split of State * State
    | Final
    override x.Equals y = System.Object.ReferenceEquals (x, y)
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? State as y -> compare (x.GetHashCode ()) (y.GetHashCode ())
            | _ -> invalidArg "yobj" "cannot compare value of different types"

module Autom =

    let rec private moveNext letter state =
        match state, letter with
        | State (Any, next)             , _        -> Some next
        | State (Letter c', next)       , Letter c          
            when c' = c                            -> Some next
        | State (Range (min, max), next), Letter c 
            when min <= c && c <= max              -> Some next
        | _                                        -> None
    
    let rec private skipEpsilons visited included toInclude =
        match toInclude with
        | [] -> included
        | state::toInclude' ->
            if visited |> Set.contains state then
                skipEpsilons visited included toInclude'
            else
                match state with
                | Split (l, r) -> skipEpsilons (visited |> Set.add state) included (l::r::toInclude')
                | _ -> skipEpsilons visited (included |> Set.add state) toInclude'

    let private consume currents letter =
       currents 
       |> Set.toList
       |> List.choose (moveNext letter) 
       |> skipEpsilons Set.empty Set.empty

    let run start = 
        Seq.map Letter 
        >> Seq.fold consume (skipEpsilons Set.empty Set.empty (List.singleton start))
        >> Set.contains Final
