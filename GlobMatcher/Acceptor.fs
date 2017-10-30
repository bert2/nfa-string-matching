module Acceptor

type Id = Id of string
type State = State of Id | Success | Failure
type Word = Word of string | Anything
type Transition = {Start: State; End: State; Accepts: Word}

let private getHead (str:string) =
    match str.Length with
    | 0 -> Word ""
    | _ -> Word str.[..0]

let private isOutgoing state {Start = start} = 
    state = start

let private accepts word transition =
    match word, transition with
    | Word x, {Accepts = Word y} when x = y -> true
    | Anything, {Accepts = Anything} -> true
    | _ -> false

let private findAccepting word transitions =
    match transitions |> List.tryFind (accepts word) with
    | None -> transitions |> List.tryFind (accepts Anything)
    | t -> t

let private consume currentState transitions word =
    match currentState with
    | Failure -> Failure
    | Success -> Failure
    | _ ->
        let outgoing = transitions |> List.filter (isOutgoing currentState)
        match outgoing |> findAccepting word with
        | Some {End = nextState} -> nextState
        | None -> Failure

let rec accept startState transitions (text:string) =
    let nextState = consume startState transitions (getHead text)
    match text.Length with
    | 0 -> nextState = Success
    | _ -> accept nextState transitions text.[1..]
