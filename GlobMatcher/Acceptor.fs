module Acceptor

type Word = Word of string | Anything
type State = State | Success | Failure
type Transition = {Start: State; End: State; Accepts: Word}

let consume currentState transitions word =
    let isOutgoingFrom state {Start = start} = state = start

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
    | Failure -> Failure
    | Success -> Failure
    | _ ->
        let outgoing = transitions |> List.filter (isOutgoingFrom currentState)
        match outgoing |> findAccepting word with
        | Some {End = nextState} -> nextState
        | None -> Failure

let rec accept startState transitions (text:string) =
    let getHead (str:string) =
        match str.Length with
        | 0 -> Word ""
        | _ -> Word str.[..0]

    let nextState = 
        text
        |> getHead
        |> consume startState transitions

    match text.Length with
    | 0 -> nextState = Success
    | _ -> 
        let tail = text.[1..]
        accept nextState transitions tail

