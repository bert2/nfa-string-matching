module TrivialPatterns

open Xunit
open GlobMatcher

[<Theory>]
[<InlineData("a", "a", true)>]
[<InlineData("a", "b", false)>]
[<InlineData("a", "", false)>]
let ``matches literal characters`` pattern text isMatch =
    let M = GlobParser.toAutomaton pattern
    let result = Automaton.run M text
    Assert.Equal(isMatch, result)

[<Theory>]
[<InlineData("?", "a", true)>]
[<InlineData("?", "", false)>]
let ``matches the any character wildcard "?"`` pattern text isMatch =
    let M = GlobParser.toAutomaton pattern
    let result = Automaton.run M text
    Assert.Equal(isMatch, result)

[<Theory>]
[<InlineData("*", "a")>]
[<InlineData("*", "abc")>]
[<InlineData("*", "")>]
let ``matches then any string of characters wildcard "*"`` pattern text =
    let M = GlobParser.toAutomaton pattern
    let result = Automaton.run M text
    Assert.True(result)

[<Theory>]
[<InlineData(@"\?", "?")>]
[<InlineData(@"\*", "*")>]
[<InlineData(@"\\", @"\")>]
let ``escape character allows matching meta characters literally`` pattern text =
    let M = GlobParser.toAutomaton pattern
    let result = Automaton.run M text
    Assert.True(result)
