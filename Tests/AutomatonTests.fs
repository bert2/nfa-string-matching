module AutomatonTests

//open Xunit
//open GlobMatcher

//[<Theory>]
//[<InlineData("", false)>]
//[<InlineData("a", true)>]
//[<InlineData("b", false)>]
//[<InlineData("aa", true)>]
//[<InlineData("ab", false)>]
//[<InlineData("aaa", false)>]
//let ``handles two transitions accepting then same word`` text matches =
//    // (0)--a->(1)--a->(2)
//    //  '-------a-------^
//    let q0 = State (UniqueId "0")
//    let q1 = State (UniqueId "1")
//    let q2 = State (UniqueId "2")
//    let t02 = {Start = q0; End = q2; Accepts = Word 'a'}
//    let t01 = {Start = q0; End = q1; Accepts = Word 'a'}
//    let t12 = {Start = q1; End = q2; Accepts = Word 'a'}
//    let M = {Initial = [q0]; Final = [q2]; Transitions = [t02; t01; t12]}

//    let result = Automaton.run M text

//    Assert.Equal (matches, result)

//[<Theory>]
//[<InlineData("", true)>]
//[<InlineData("a", false)>]
//let ``expands intial states across epsilon transitions`` text matches =
//    // (0)--->(1)
//    let q0 = State (UniqueId "0")
//    let q1 = State (UniqueId "1")
//    let t01 = {Start = q0; End = q1; Accepts = Epsilon}
//    let M = {Initial = [q0]; Final = [q1]; Transitions = [t01]}

//    let result = Automaton.run M text

//    Assert.Equal (matches, result)

//[<Fact>]
//let ``expands intermediate states across epsilon transitions`` () =
//    // (0)--a->(1)---->(2)--a->(3)
//    let q0 = State (UniqueId "0")
//    let q1 = State (UniqueId "1")
//    let q2 = State (UniqueId "2")
//    let q3 = State (UniqueId "3")
//    let t01 = {Start = q0; End = q1; Accepts = Word 'a'}
//    let t12 = {Start = q1; End = q2; Accepts = Epsilon}
//    let t23 = {Start = q2; End = q3; Accepts = Word 'a'}
//    let M = {Initial = [q0]; Final = [q3]; Transitions = [t01; t12; t23]}

//    let result = Automaton.run M "aa"

//    Assert.True result

//[<Fact>]
//let ``expands states across epsilon transitions recursively`` () =
//    // (0)---->(1)---->(2)
//    let q0 = State (UniqueId "0")
//    let q1 = State (UniqueId "1")
//    let q2 = State (UniqueId "2")
//    let t01 = {Start = q0; End = q1; Accepts = Epsilon}
//    let t12 = {Start = q1; End = q2; Accepts = Epsilon}
//    let M = {Initial = [q0]; Final = [q2]; Transitions = [t01; t12]}

//    let result = Automaton.run M ""

//    Assert.True result

//[<Fact>]
//let ``recursive epsilon expansion stops when looping back directly`` () =
//    // /¯\
//    // \ V
//    // (0)
//    let q0 = State (UniqueId "0")
//    let t00 = {Start = q0; End = q0; Accepts = Epsilon}
//    let M = {Initial = [q0]; Final = [q0]; Transitions = [t00]}

//    let result = Automaton.run M ""

//    Assert.True result

//[<Fact>]
//let ``recursive epsilon expansion stops when looping back indirectly`` () =
//    // (0)---->(1)
//    //  ^-------'
//    let q0 = State (UniqueId "0")
//    let q1 = State (UniqueId "1")
//    let t01 = {Start = q0; End = q1; Accepts = Epsilon}
//    let t10 = {Start = q1; End = q0; Accepts = Epsilon}
//    let M = {Initial = [q0]; Final = [q1]; Transitions = [t01; t10]}

//    let result = Automaton.run M ""

//    Assert.True(result)

//[<Theory>]
//[<InlineData("b", true)>]
//[<InlineData("bb", true)>]
//[<InlineData("", false)>]
//[<InlineData("a", false)>]
//[<InlineData("ba", false)>]
//[<InlineData("ab", false)>]
//let ``will fail when no matching transition is found`` text matches =
//    // (0)--b->(1)
//    //  ^-------'
//    let q0 = State (UniqueId "0")
//    let q1 = State (UniqueId "1")
//    let t01 = {Start = q0; End = q1; Accepts = Word 'b'}
//    let t10 = {Start = q1; End = q0; Accepts = Epsilon}
//    let M = {Initial = [q0]; Final = [q1]; Transitions = [t01; t10]}

//    let result = Automaton.run M text

//    Assert.Equal (matches, result)

//[<Theory>]
//[<InlineData("", true)>]
//[<InlineData("a", true)>]
//[<InlineData("b", false)>]
//[<InlineData("ab", false)>]
//let ``supports multiple initial states`` text matches =
//    // (0)   (0')---->(1)
//    //  '------a-------^
//    let q0 = State (UniqueId "0")
//    let q0' = State (UniqueId "0'")
//    let q1 = State (UniqueId "1")
//    let t01 = {Start = q0; End = q1; Accepts = Word 'a'}
//    let t0'1 = {Start = q0'; End = q1; Accepts = Epsilon}
//    let M = {Initial = [q0; q0']; Final = [q1]; Transitions = [t01; t0'1]}

//    let result = Automaton.run M text

//    Assert.Equal (matches, result)
