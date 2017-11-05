module BugFixedPatterns

open Xunit
open GlobMatcher

[<Theory>]
[<InlineData("abb")>]
[<InlineData("abbb")>]
[<InlineData("ababb")>]
[<InlineData("abbabb")>]
[<InlineData("abbbabb")>]
let ``two equal characters after a "*" should not break backtracking`` text =
    let start::_,transitions = Parser.toAcceptor "*bb"
    let result = Acceptor.run start transitions text
    Assert.True(result)

[<Theory>]
[<InlineData("abb")>]
[<InlineData("abbb")>]
[<InlineData("abc")>]
[<InlineData("abbc")>]
[<InlineData("abcbc")>]
[<InlineData("abcabc")>]
let ``should handle ambiguities due to patterns like "*b?"`` text =
    let start::_,transitions = Parser.toAcceptor "*b?"
    let result = Acceptor.run start transitions text
    Assert.True(result)

[<Theory>]
[<InlineData("bcac")>]
[<InlineData("cbcac")>]
[<InlineData("cbbbaaa")>]
[<InlineData("cbcabcac")>]
[<InlineData("cbcaccbcac")>]
[<InlineData("cbacccbcac")>]
let ``should handle multiple "?" after a "*"`` text =
    let start::_,transitions = Parser.toAcceptor "*b?a?"
    let result = Acceptor.run start transitions text
    Assert.True(result)