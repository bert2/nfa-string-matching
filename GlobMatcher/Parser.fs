module Parser

open Acceptor
open Util

let private findTransition startState endState =
    List.find (fun {Start = s; End = e} -> s = startState && e = endState)

let private extentWithFixedLengthWord (oldStart::states) transitions word =
    let newStart = State (shortId () |> UniqueId)
    let t = {Start = newStart; End = oldStart; Accepts = word}
    newStart::oldStart::states, t::transitions

let private extentWithVariableLengthWord (fst::snd::states) transitions =
    let t1 = {Start = fst; End = fst; Accepts = Anything}
    let transitions' =
        match snd with
        | Success -> t1::transitions
        | _ ->
            let backtrack = {Start = snd; End = fst; Accepts = Anything}
            let {Accepts = word} = transitions |> findTransition fst snd
            let t3 = {Start = snd; End = snd; Accepts = word}
            t1::backtrack::t3::transitions
    fst::snd::states, transitions'

let private parseWord states transitions word =
    match word with
    | "*" -> extentWithVariableLengthWord states transitions
    | "?" -> extentWithFixedLengthWord states transitions Anything
    | _ -> extentWithFixedLengthWord states transitions (Word word)
    
let rec private parsePattern (start::states) transitions (pattern:string) =
    match pattern.Length with
    | 0 -> start, transitions
    | _ -> 
        let states', transistions' = parseWord (start::states) transitions pattern.[..0]
        parsePattern states' transistions' pattern.[1..]

let toAcceptor pattern =
    let initial = State (shortId () |> UniqueId)
    let halt = Success
    let eof = {Start = initial; End = halt; Accepts = Word ""}
    parsePattern [initial; halt] [eof] (reverse pattern)
