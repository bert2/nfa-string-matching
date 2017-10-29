module Acceptor

type State = Success | State of Transition list
and Transition = { Target: State; Accepts: Word }
and Word = Word of string | Anything

let consume currentState word =
    let accepts word transition =
        match word, transition with
        | Word x, {Accepts = Word y} when x = y -> true
        | Anything, {Accepts = Anything} -> true
        | _ -> false

    let findAccepting word transitions = 
        match transitions |> List.tryFind (accepts word) with
        | None -> transitions |> List.tryFind (accepts Anything)
        | t -> t

    match currentState with
    | Success -> currentState
    | State transitions ->
        match transitions |> findAccepting word with
        | Some {Target = target} -> target
        | None -> currentState
