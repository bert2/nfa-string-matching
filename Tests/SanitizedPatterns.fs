module SanitizedPatterns

open Xunit
open GlobMatcher

type IdMock() =
    let mutable id = 0
    member __.Generator () = 
        id <- id + 1
        sprintf "%i" id

[<Fact>]
let ``pattern "**" yields then same state machine as pattern "*"`` () =
    let ids = IdMock().Generator
    let states,transistions = Parser.toAcceptorWithStateId ids "**"

    let ids' = IdMock().Generator
    let states',transistions' = Parser.toAcceptorWithStateId ids' "*"

    Assert.Equal<State list>(states, states')
    Assert.Equal<Transition list>(transistions, transistions')

[<Fact>]
let ``pattern "*?" yields then same state machine as pattern "?*"`` () =
    let ids = IdMock().Generator
    let states,transistions = Parser.toAcceptorWithStateId ids "*?"

    let ids' = IdMock().Generator
    let states',transistions' = Parser.toAcceptorWithStateId ids' "?*"

    Assert.Equal<State list>(states, states')
    Assert.Equal<Transition list>(transistions, transistions')
