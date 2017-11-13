module NontrivialPatterns

open Xunit
open GlobMatcher

[<Theory>]
[<InlineData("abb")>]
[<InlineData("abbb")>]
[<InlineData("ababb")>]
[<InlineData("abbabb")>]
[<InlineData("abbbabb")>]
let ``two equal characters after a "*" should not break backtracking`` text =
    let M = GlobParser.toAutomaton "*bb"
    let result = Automaton.run M text
    Assert.True(result)

[<Theory>]
[<InlineData("abb")>]
[<InlineData("abbb")>]
[<InlineData("abc")>]
[<InlineData("abbc")>]
[<InlineData("abcbc")>]
[<InlineData("abcabc")>]
let ``should handle ambiguities due to patterns like "*b?"`` text =
    let M = GlobParser.toAutomaton "*b?"
    let result = Automaton.run M text
    Assert.True(result)

[<Theory>]
[<InlineData("bcac")>]
[<InlineData("cbcac")>]
[<InlineData("cbbbaaa")>]
[<InlineData("cbcabcac")>]
[<InlineData("cbcaccbcac")>]
[<InlineData("cbacccbcac")>]
let ``should handle multiple "?" after a "*"`` text =
    let M = GlobParser.toAutomaton "*b?a?"
    let result = Automaton.run M text
    Assert.True(result)

[<Theory>]
[<InlineData("cbcaca")>]
let ``should backtrack across multiple "?"`` text =
    let M = GlobParser.toAutomaton "*b?a?"
    let result = Automaton.run M text
    Assert.False(result)