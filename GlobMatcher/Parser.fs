﻿namespace GlobMatcher

module Parser = 

    open System
    open Util

    let private findTransition startState endState =
        List.find (fun {Start = s; End = e} -> s = startState && e = endState)

    let private accepts word state =
        List.exists (fun {Start = s; Accepts = w} -> s = state && w = word)

    let private extentWithFixedLengthWord idGenerator (oldStart::states) transitions word =
        let newStart = State (idGenerator () |> UniqueId)
        let t = {Start = newStart; End = oldStart; Accepts = word}
        newStart::oldStart::states, t::transitions

    let rec private extendWithBacktracking target firstSuccessor exitWord remainingSuccessors allSuccessors transitions =
        match remainingSuccessors with
        | [] -> transitions
        | [Success] -> transitions
        | state::successors' ->
            if not (transitions |> accepts Anything state) then
                let backtrack = {Start = state; End = target; Accepts = Anything}
                if not (transitions |> accepts exitWord state) then
                    let holdTarget = allSuccessors |> List.find (fun s -> not (transitions |> accepts exitWord s))
                    let hold = {Start = state; End = holdTarget; Accepts = exitWord}
                    backtrack::hold::(extendWithBacktracking target firstSuccessor exitWord successors' allSuccessors transitions)
                else
                    backtrack::(extendWithBacktracking target firstSuccessor exitWord successors' allSuccessors transitions)
            else
                transitions // We reached another "*" or a "?" -- backtracking can stop here.

    let private extentWithVariableLengthWord (fst::snd::states) transitions =
        let acceptLoop = {Start = fst; End = fst; Accepts = Anything}
        let {Accepts = exitWord} = transitions |> findTransition fst snd
        let transitions' = extendWithBacktracking fst snd exitWord (snd::states) (snd::states) transitions
        fst::snd::states, acceptLoop::transitions'

    let private parseWord idGenerator states transitions word =
        match word with
        | "*" -> extentWithVariableLengthWord states transitions
        | "?" -> extentWithFixedLengthWord idGenerator states transitions Anything
        | _ -> extentWithFixedLengthWord idGenerator states transitions (Word word)
    
    let rec private parsePattern idGenerator (start::states) transitions (pattern:string) =
        match pattern.Length with
        | 0 -> start::states, transitions
        | _ -> 
            let states', transistions' = parseWord idGenerator (start::states) transitions pattern.[..0]
            parsePattern idGenerator states' transistions' pattern.[1..]

    let rec private sanitize (pattern:string) =
        if pattern.Contains("**") then
            sanitize (pattern.Replace("**", "*"))
        elif pattern.Contains("*?") then
            sanitize (pattern.Replace("*?", "?*"))
        else
            pattern

    let toAcceptorWithStateId idGenerator pattern =
        let initial = State (idGenerator () |> UniqueId)
        let halt = Success
        let eof = {Start = initial; End = halt; Accepts = Word ""}
        let pattern' = pattern |> sanitize |> reverse
        parsePattern idGenerator [initial; halt] [eof] pattern'

    let toAcceptor = toAcceptorWithStateId shortId 

