module AutomatonBuilder

open Util
open GlobMatcher

let makeChar c =
    let q0 = State (UniqueId <| shortId (), Continue)
    let q1 = State (UniqueId <| shortId (), Accept)
    let t = {Start = q0; End = q1; Accepts = Word c}
    Automaton ([q0], [t])