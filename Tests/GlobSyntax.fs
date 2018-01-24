module GlobSyntax

open Xunit
open StringMatcher

let private assertMatch pattern text isMatch =
    let a = GlobParser.toAutomaton' pattern
    let result = Autom.run a text
    Assert.Equal (isMatch, result)

[<Theory>]
[<InlineData("a", "a", true)>]
[<InlineData("a", "b", false)>]
[<InlineData("a", "", false)>]
let ``matches literal characters`` p t r = assertMatch p t r


[<Theory>]
[<InlineData("?", "a", true)>]
[<InlineData("?", "", false)>]
let ``matches the any character wildcard "?"`` p t r = assertMatch p t r

[<Theory>]
[<InlineData("*", "a", true)>]
[<InlineData("*", "abc", true)>]
[<InlineData("*", "", true)>]
let ``matches then any string wildcard "*"`` p t r = assertMatch p t r

[<Theory>]
[<InlineData(@"[b-d]", "a", false)>]
[<InlineData(@"[b-d]", "b", true)>]
[<InlineData(@"[b-d]", "c", true)>]
[<InlineData(@"[b-d]", "d", true)>]
[<InlineData(@"[b-d]", "e", false)>]
let ``matches character ranges`` p t r = assertMatch p t r

[<Theory>]
[<InlineData(@"\?", "?", true)>]
[<InlineData(@"\*", "*", true)>]
[<InlineData(@"\\", @"\", true)>]
[<InlineData(@"\[", "[", true)>]
[<InlineData(@"\]", "]", true)>]
let ``escape character allows matching meta characters literally`` p t r = assertMatch p t r


[<Theory>]
[<InlineData(@"*b", "b", true)>]
[<InlineData(@"*b", "ab", true)>]
[<InlineData(@"*b", "aab", true)>]
[<InlineData(@"*b", "c", false)>]
[<InlineData(@"a*c?[0-9]\?*?h", "abcd1?efgh", true)>]
[<InlineData(@"a*c?[0-9]\?*?h", "acd1?gh", true)>]
let ``can combine automata``  p t r = assertMatch p t r

[<Theory>]
[<InlineData(@"\")>]
[<InlineData(@"\a")>]
[<InlineData("[")>]
[<InlineData("[a]")>]
[<InlineData("[a-]")>]
let ``invalid pattern gives parser error`` pattern =
    match GlobParser.toAutomaton pattern with
    | Result.Failure _ -> ()
    | Result.Success _ -> failwith "Expected parser error."
