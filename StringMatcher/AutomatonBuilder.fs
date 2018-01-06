namespace StringMatcher

module AutomatonBuilder =
    
    open Util

    // Automata are build by chaining proto automatons together backwards. A proto
    // automaton is a (partial) automaton missing the transition to an exit state. 
    // When a proto automaton is completed by fixing its exit state, its initial 
    // state is returned which in turn can be used as the exit state of another 
    // proto automaton.

    type ProtoAutomaton = ProtoAutomaton of (State -> State)

    let complete (ProtoAutomaton fix) exit = fix exit

    // ProtoAutomaton is monoidal and can be folded over backwards.

    let connect first second = ProtoAutomaton (complete second >> complete first)

    let zero = ProtoAutomaton id

    // Helpers & generalizations

    let newId = globalCount >> string >> Id

    let private accept letter exit = State (newId (), letter, exit)

    let private loop body exit =
        let rec branch = Split (newId (), enter, exit)
        and enter = complete body branch
        branch

    let private option inner skip =
        let take = complete inner skip
        Split (newId (), take, skip)

    // Implementation of control structures

    let makeChar = ProtoAutomaton << accept << Letter

    let makeAnyChar () = ProtoAutomaton << accept <| Any

    let makeRange = ProtoAutomaton << accept << Range << sortTuple

    let makeZeroOrMore = ProtoAutomaton << loop

    let makeOneOrMore inner = ProtoAutomaton (complete inner << loop inner)

    let makeZeroOrOne = ProtoAutomaton << option   
