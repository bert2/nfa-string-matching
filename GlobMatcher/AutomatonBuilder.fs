module AutomatonBuilder

open Util
open GlobMatcher

let makeChar c =
    let q0 = State (UniqueId <| shortId ())
    let q1 = State (UniqueId <| shortId ())
    let t = {Start = q0; End = q1; Accepts = Word c}
    Automaton ([q0], [q1], [t])