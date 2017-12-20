namespace GlobMatcher

module AutomatonBuilder =

    type Prototype = Prototype of (State -> State)

    let private newId = 
        let mutable i = -1
        fun () ->
            i <- i + 1
            i  |> string |> Id

    let run (Prototype finish) next = finish next

    let empty = Final

    let makeChar c =
        Prototype (fun next -> 
            State (newId (), Word c, next))

    let makeAnyChar () =
        Prototype (fun next ->
            State (newId (), Any, next))

    let makeAnyString () =
        Prototype (fun next ->
            let rec s1 = State (newId (), Any, s0)
            and s0 = Split (newId (), s1, next)
            s0)

    let makeZeroOrMoreChar c =
        Prototype (fun next ->
            let rec s0 = Split (newId (), s1, next)
            and s1 = State (newId (), Word c, s0)
            s0)

    let makeOneOrMoreChar c =
        Prototype (fun next ->
            let rec s0 = State (newId (), Word c, s1)
            and s1 = Split (newId (), s0, next)
            s0)

    let makeZeroOrOneChar c =
        Prototype (fun next ->
            let s1 = State (newId (), Word c, next)
            let s0 = Split (newId (), s1, next)
            s0)

    let makeRange (minChar, maxChar) =
        Prototype (fun next ->
            let minChar' = min minChar maxChar
            let maxChar' = max minChar maxChar
            let s = State (newId (), Range (minChar', maxChar'), next)
            s)    
