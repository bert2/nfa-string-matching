module AutomatonTests

open Xunit
open GlobMatcher

[<Fact>]
let ``handles two transitions accepting then same word`` () =
    let q0 = State (UniqueId "0", Continue)
    let q1 = State (UniqueId "1", Continue)
    let q2 = State (UniqueId "2", Accept)
    let ``one a`` = {Start = q0; End = q2; Accepts = Word 'a'}
    let ``two a (1)`` = {Start = q0; End = q1; Accepts = Word 'a'}
    let ``two a (2)`` = {Start = q1; End = q2; Accepts = Word 'a'}
    let M = Automaton (q0, [``one a``; ``two a (1)``; ``two a (2)``])

    Assert.True(Automaton.run M "a", "rejected 'a'")
    Assert.True(Automaton.run M "aa", "rejected 'aa'")

[<Fact>]
let ``expands intial states across epsilon transitions`` () =
    let q0 = State (UniqueId "0", Continue)
    let q1 = State (UniqueId "1", Accept)
    let t01 = {Start = q0; End = q1; Accepts = Epsilon}
    let M = Automaton (q0, [t01])

    let result = Automaton.run M ""

    Assert.True(result)

[<Fact>]
let ``expands intermediate states across epsilon transitions`` () =
    let q0 = State (UniqueId "0", Continue)
    let q1 = State (UniqueId "1", Continue)
    let q2 = State (UniqueId "2", Continue)
    let q3 = State (UniqueId "3", Accept)
    let t01 = {Start = q0; End = q1; Accepts = Word 'a'}
    let t12 = {Start = q1; End = q2; Accepts = Epsilon}
    let t23 = {Start = q2; End = q3; Accepts = Word 'a'}
    let M = Automaton (q0, [t01; t12; t23])

    let result = Automaton.run M "aa"

    Assert.True(result)

[<Fact>]
let ``expands states across epsilon transitions recursively`` () =
    let q0 = State (UniqueId "0", Continue)
    let q1 = State (UniqueId "1", Continue)
    let q2 = State (UniqueId "2", Accept)
    let t01 = {Start = q0; End = q1; Accepts = Epsilon}
    let t12 = {Start = q1; End = q2; Accepts = Epsilon}
    let M = Automaton (q0, [t01; t12])

    let result = Automaton.run M ""

    Assert.True(result)

[<Fact>]
let ``recursive epsilon expansion stops when looping back directly`` () =
    let q0 = State (UniqueId "0", Accept)
    let t00 = {Start = q0; End = q0; Accepts = Epsilon}
    let M = Automaton (q0, [t00])

    let result = Automaton.run M ""

    Assert.True(result)

[<Fact>]
let ``recursive epsilon expansion stops when looping back indirectly`` () =
    let q0 = State (UniqueId "0", Continue)
    let q1 = State (UniqueId "1", Accept)
    let t01 = {Start = q0; End = q1; Accepts = Epsilon}
    let t10 = {Start = q1; End = q0; Accepts = Epsilon}
    let M = Automaton (q0, [t01; t10])

    let result = Automaton.run M ""

    Assert.True(result)

[<Fact>]
let ``will enter failure state when no matching transition is found`` () =
    let q0 = State (UniqueId "0", Continue)
    let q1 = State (UniqueId "1", Accept)
    let ``b`` = {Start = q0; End = q1; Accepts = Word 'b'}
    let ``epsilon`` = {Start = q1; End = q0; Accepts = Epsilon}
    let M = Automaton (q0, [``b``; ``epsilon``])

    let result = Automaton.run M "aab"

    Assert.False(result)

// Delete the tests below as soon as we can build/parse automatons.

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