namespace GlobMatcher

module AutomatonBuilder =
    
    open Util

    // Automatons are build by chaining one or more proto automatons together backwards. A proto
    // automaton is a (partial) automaton missing the transition to an exit state. When a proto 
    // automaton is completed by feeding it its exit state, its initial state is returned which 
    // in turn could be the exit state of another proto automaton.

    type ProtoAutomaton = ProtoAutomaton of (State -> State)

    let complete (ProtoAutomaton completeWith) exit = completeWith exit

    // Make ProtoAutomaton a monoid w/o associativity (whatever that is called... a quasigroup?).

    let combine first second = ProtoAutomaton (complete first << complete second)

    let zero = ProtoAutomaton id

    // Generalizations

    let newId = count >> string >> Id

    let private accept word exit = State (newId (), word, exit)

    let private loop body exit =
        let rec branch = Split (newId (), enter, exit)
        and enter = complete body branch
        branch

    let private option inner skip =
        let take = complete inner skip
        Split (newId (), take, skip)

    // Implementation of control structures

    let makeChar = ProtoAutomaton << accept << Word

    let makeAnyChar () = ProtoAutomaton << accept <| Any

    let makeRange = ProtoAutomaton << accept << Range << sortTuple

    let makeZeroOrMore = ProtoAutomaton << loop

    let makeOneOrMore inner = ProtoAutomaton (complete inner << loop inner)

    let makeZeroOrOne = ProtoAutomaton << option   
