module SimpleTests

open Xunit
open Parser
open Acceptor

[<Fact>]
let ``pattern "a" accepts "a" character`` () =
    let start,transitions = toAcceptor "a"
    let result = accept start transitions "a"
    Assert.True(result)

[<Fact>]
let ``pattern "a" rejects "b" character`` () =
    let start,transitions = toAcceptor "a"
    let result = accept start transitions "b"
    Assert.False(result)

[<Fact>]
let ``pattern "a" rejects no character`` () =
    let start,transitions = toAcceptor "a"
    let result = accept start transitions ""
    Assert.False(result)

[<Fact>]
let ``pattern "?" accepts any character`` () =
    let start,transitions = toAcceptor "?"
    let result = accept start transitions "a"
    Assert.True(result)

[<Fact>]
let ``pattern "?" rejects no character`` () =
    let start,transitions = toAcceptor "?"
    let result = accept start transitions ""
    Assert.False(result)

[<Fact>]
let ``pattern "*" accepts any character`` () =
    let start,transitions = toAcceptor "*"
    let result = accept start transitions "a"
    Assert.True(result)

[<Fact>]
let ``pattern "*" accepts any string of characters`` () =
    let start,transitions = toAcceptor "*"
    let result = accept start transitions "abc"
    Assert.True(result)

[<Fact>]
let ``pattern "*" accepts no character`` () =
    let start,transitions = toAcceptor "*"
    let result = accept start transitions ""
    Assert.True(result)
