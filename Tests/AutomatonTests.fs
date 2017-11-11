module AutomatonTests

open Xunit
open GlobMatcher

let empty () =
    let q0 = State (UniqueId "0", Accept)
    Automaton (q0, [])

let singleChar c  =
    let q0 = State (UniqueId "0", Continue)
    let q1 = State (UniqueId "1", Accept)
    let t = {Start = q0; End = q1; Accepts = Word c}
    Automaton (q0, [t])

let repeatChar c =
    let q0 = State (UniqueId "0", Continue)
    let q1 = State (UniqueId "1", Accept)
    let t = {Start = q0; End = q0; Accepts = Word c}
    let t' = {Start = q0; End = q1; Accepts = Word c}
    let t'' = {Start = q0; End = q1; Accepts = Epsilon}
    Automaton (q0, [t; t'; t''])

[<Fact>]
let ``empty automaton accepts empty string`` () =
    let M = empty ()
    let result = Automaton.run M ""
    Assert.True(result)

[<Fact>]
let ``empty automaton rejects nonempty string`` () =
    let M = empty ()
    let result = Automaton.run M "a"
    Assert.False(result)

[<Fact>]
let ``automaton for regex "a" accepts "a"`` () =
    let M = singleChar 'a'
    let result = Automaton.run M "a"
    Assert.True(result)

[<Fact>]
let ``automaton for regex "a" rejects "b"`` () =
    let M = singleChar 'a'
    let result = Automaton.run M "b"
    Assert.False(result)

[<Fact>]
let ``an automaton for regex "a*" accepts "aaa"`` () =
    let M = repeatChar 'a'
    let result = Automaton.run M "aaa"
    Assert.True(result)

[<Fact>]
let ``an automaton for regex "a*" accepts "a"`` () =
    let M = repeatChar 'a'
    let result = Automaton.run M "a"
    Assert.True(result)

[<Fact>]
let ``an automaton for regex "a*" accepts ""`` () =
    let M = repeatChar 'a'
    let result = Automaton.run M ""
    Assert.True(result)

[<Fact>]
let ``an automaton for regex "a*" rejects "b"`` () =
    let M = repeatChar 'a'
    let result = Automaton.run M "b"
    Assert.False(result)

[<Fact>]
let ``an automaton for regex "a*" rejects "ab"`` () =
    let M = repeatChar 'a'
    let result = Automaton.run M "ab"
    Assert.False(result)