namespace GlobMatcher

module Parser = 

    open System
    open Util

    let private findTransition startState endState =
        List.find (fun {Start = s; End = e} -> s = startState && e = endState)

    let private acceptsAnything state =
        List.exists (fun {Start = s; Accepts = w} -> s = state && w = Anything)

    let private extentWithFixedLengthWord idGenerator (oldStart::states) transitions word =
        let newStart = State (idGenerator () |> UniqueId)
        let t = {Start = newStart; End = oldStart; Accepts = word}
        newStart::oldStart::states, t::transitions

    let rec private extendWithBacktracking target firstSuccessor exitWord successors transitions =
        match successors with
        | [] -> transitions
        | [Success] -> transitions
        | state::successors' ->
            if not (acceptsAnything state transitions) then
                let backtrack = {Start = state; End = target; Accepts = Anything}
                let hold = {Start = state; End = firstSuccessor; Accepts = exitWord}
                backtrack::hold::(extendWithBacktracking target firstSuccessor exitWord successors' transitions)
            else
                transitions // We reached another "*" -- backtracking can stop here.

    let private extentWithVariableLengthWord (fst::snd::states) transitions =
        let acceptLoop = {Start = fst; End = fst; Accepts = Anything}
        let {Accepts = exitWord} = transitions |> findTransition fst snd
        let transitions' = extendWithBacktracking fst snd exitWord (snd::states) transitions
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
        parsePattern idGenerator [initial; halt] [eof] (pattern |> sanitize |> reverse)

    let toAcceptor = toAcceptorWithStateId shortId 

