module SimpleTests

open Xunit
open GlobMatcher

[<Fact>]
let ``pattern "a" accepts "a" character`` () =
    let start::_,transitions = Parser.toAcceptor "a"
    let result = Acceptor.run start transitions "a"
    Assert.True(result)

[<Fact>]
let ``pattern "a" rejects "b" character`` () =
    let start::_,transitions = Parser.toAcceptor "a"
    let result = Acceptor.run start transitions "b"
    Assert.False(result)

[<Fact>]
let ``pattern "a" rejects no character`` () =
    let start::_,transitions = Parser.toAcceptor "a"
    let result = Acceptor.run start transitions ""
    Assert.False(result)

[<Fact>]
let ``pattern "?" accepts any character`` () =
    let start::_,transitions = Parser.toAcceptor "?"
    let result = Acceptor.run start transitions "a"
    Assert.True(result)

[<Fact>]
let ``pattern "?" rejects no character`` () =
    let start::_,transitions = Parser.toAcceptor "?"
    let result = Acceptor.run start transitions ""
    Assert.False(result)

[<Fact>]
let ``pattern "*" accepts any character`` () =
    let start::_,transitions = Parser.toAcceptor "*"
    let result = Acceptor.run start transitions "a"
    Assert.True(result)

[<Fact>]
let ``pattern "*" accepts any string of characters`` () =
    let start::_,transitions = Parser.toAcceptor "*"
    let result = Acceptor.run start transitions "abc"
    Assert.True(result)

[<Fact>]
let ``pattern "*" accepts no character`` () =
    let start::_,transitions = Parser.toAcceptor "*"
    let result = Acceptor.run start transitions ""
    Assert.True(result)
