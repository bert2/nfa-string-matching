namespace StringMatcher

module AutomatonBuilder =
    
    open Util

    // Automata are build by chaining proto automata together backwards. A proto
    // automaton is a partial automaton missing the transition to an exit state. 
    // When a proto automaton is completed by fixing its exit state, its initial 
    // state is returned which in turn can be used as the exit state of another 
    // proto automaton.
    type ProtoAutomaton = ProtoAutomaton of (State -> State)
    let complete (ProtoAutomaton fix) exit = fix exit

    // The type 'ProtoAutomaton' together with function 'connect' is a monoid and 
    // can be folded over *backwards* using the identity element 'empty'.
    let connect first second = ProtoAutomaton (complete second >> complete first)
    let empty = ProtoAutomaton id

    // Generalizations

    let private accept letter exit = State (letter, exit)

    let private loop body exit =
        let rec split = Split (enter, exit)
        and enter = complete body split
        split

    let private branch left right join = Split (complete left join, complete right join)

    // Implementation of control structures
    let makeChar            = ProtoAutomaton << accept << Letter
    let makeAnyChar ()      = ProtoAutomaton << accept <| Any
    let makeRange           = ProtoAutomaton << accept << Range << sortTuple
    let makeZeroOrMore      = ProtoAutomaton << loop
    let makeOneOrMore inner = ProtoAutomaton (complete inner << loop inner)
    let makeZeroOrOne       = ProtoAutomaton << branch empty
    let makeAlternation l r = ProtoAutomaton <| branch l r
