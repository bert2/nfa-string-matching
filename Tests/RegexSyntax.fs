module RegexSyntax

open Xunit
open GlobMatcher

[<Theory>]
[<InlineData("b", "b", true)>]
[<InlineData("b", "a", false)>]
[<InlineData("b", "", false)>]
[<InlineData("b", "ab", true)>]
[<InlineData("b", "bc", true)>]
[<InlineData("b", "abc", true)>]
let ``matches literal characters`` pattern text isMatch =
    let M = RegexParser.toAutomaton' pattern
    let result = Automaton.run M text
    Assert.Equal (isMatch, result)
