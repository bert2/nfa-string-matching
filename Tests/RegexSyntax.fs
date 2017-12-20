module RegexSyntax

open Xunit
open GlobMatcher

[<Theory>]
[<InlineData("b", "b", true)>]
[<InlineData("b", "a", false)>]
[<InlineData("b", "", false)>]
let ``matches literal characters`` pattern text isMatch =
    let M = RegexParser.toAutomaton' pattern
    let result = Automaton.run M text
    Assert.Equal (isMatch, result)

[<Theory>]
[<InlineData("a*", "", true)>]
[<InlineData("a*", "a", true)>]
[<InlineData("a*", "aa", true)>]
let ``Kleene star matches zero or more characters`` pattern text isMatch =
    let M = RegexParser.toAutomaton' pattern
    let result = Automaton.run M text
    Assert.Equal (isMatch, result)

[<Theory>]
[<InlineData("a+", "", false)>]
[<InlineData("a+", "a", true)>]
[<InlineData("a+", "aa", true)>]
let ``Kleene plus matches one or more characters`` pattern text isMatch =
    let M = RegexParser.toAutomaton' pattern
    let result = Automaton.run M text
    Assert.Equal (isMatch, result)

[<Theory>]
[<InlineData("a?", "", true)>]
[<InlineData("a?", "a", true)>]
let ``option matches zero or one character`` pattern text isMatch =
    let M = RegexParser.toAutomaton' pattern
    let result = Automaton.run M text
    Assert.Equal (isMatch, result)

[<Theory>]
[<InlineData("(a)", "a", true)>]
[<InlineData("(a)", "b", false)>]
[<InlineData("(ab)", "ab", true)>]
[<InlineData("a(bc)", "abc", true)>]
[<InlineData("a(bc)d", "abcd", true)>]
[<InlineData("a(bc)d", "ad", false)>]
let ``parantheses form subexpressions`` pattern text isMatch =
    let M = RegexParser.toAutomaton' pattern
    let result = Automaton.run M text
    Assert.Equal (isMatch, result)

[<Theory>]
[<InlineData("a*b", "b")>]
[<InlineData("a*b", "ab")>]
[<InlineData("a*b", "aab")>]
[<InlineData("ab*c+d?e", "abbbcccde")>]
[<InlineData("ab*c+d?e", "acccde")>]
[<InlineData("ab*c+d?e", "abbbccce")>]
[<InlineData("ab*c+d?e", "accce")>]
let ``can combine automatons`` pattern text =
    let M = RegexParser.toAutomaton' pattern
    let result = Automaton.run M text
    Assert.True result