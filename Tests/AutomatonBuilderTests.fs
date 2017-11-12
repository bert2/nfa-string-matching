module AutomatonBuilderTests

open Xunit
open GlobMatcher

[<Fact>]
let ``can concatenate two single char automatons`` () =
    let M1 = AutomatonBuilder.makeChar 'a'
    let M2 = AutomatonBuilder.makeChar 'b'
    
    let M = AutomatonBuilder.concat M1 M2

    Assert.True(Automaton.run M "ab")