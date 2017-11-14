module TrivialPatterns

open Xunit
open GlobMatcher

[<Fact>]
let ``pattern "a" accepts "a" character`` () =
    let M = GlobParser.toAutomaton "a"
    let result = Automaton.run M "a"
    Assert.True(result)

[<Fact>]
let ``pattern "a" rejects "b" character`` () =
    let M = GlobParser.toAutomaton "a"
    let result = Automaton.run M "b"
    Assert.False(result)

[<Fact>]
let ``pattern "a" rejects no character`` () =
    let M = GlobParser.toAutomaton "a"
    let result = Automaton.run M ""
    Assert.False(result)

[<Fact>]
let ``pattern "?" accepts any character`` () =
    let M = GlobParser.toAutomaton "?"
    let result = Automaton.run M "a"
    Assert.True(result)

[<Fact>]
let ``pattern "?" rejects no character`` () =
    let M = GlobParser.toAutomaton "?"
    let result = Automaton.run M ""
    Assert.False(result)

[<Fact>]
let ``pattern "*" accepts any character`` () =
    let M = GlobParser.toAutomaton "*"
    let result = Automaton.run M "a"
    Assert.True(result)

[<Fact>]
let ``pattern "*" accepts any string of characters`` () =
    let M = GlobParser.toAutomaton "*"
    let result = Automaton.run M "abc"
    Assert.True(result)

[<Fact>]
let ``pattern "*" accepts no character`` () =
    let M = GlobParser.toAutomaton "*"
    let result = Automaton.run M ""
    Assert.True(result)

[<Fact>]
let ``escape character allows matching "?" literally`` () =
    let M = GlobParser.toAutomaton @"\?"
    let result = Automaton.run M "?"
    Assert.True(result)

[<Fact>]
let ``escape character allows matching "*" literally`` () =
    let M = GlobParser.toAutomaton @"\*"
    let result = Automaton.run M "*"
    Assert.True(result)

[<Fact>]
let ``escape character allows matching "\" literally`` () =
    let M = GlobParser.toAutomaton @"\\"
    let result = Automaton.run M @"\"
    Assert.True(result)