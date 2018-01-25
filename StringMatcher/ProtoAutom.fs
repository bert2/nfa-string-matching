namespace StringMatcher

module ProtoAutom =
    
    open Util

    // Automata are build by chaining proto automata together backwards. A proto
    // automaton is a partial automaton missing the transition to an exit state. 
    // When a proto automaton is completed by fixing its exit state, its initial 
    // state is returned which in turn can be used as the exit state of another 
    // proto automaton.
    type ProtoAutom = ProtoAutom of (State -> State)
    let complete (ProtoAutom fix) exit = fix exit

    // Proto automata form a monoid under 'connect' with 'empty' as identity 
    // element. Folding over them *backwards* assembles a combined automaton.
    let empty = ProtoAutom id
    let connect first second = ProtoAutom (complete second >> complete first)
    let concat = List.foldBack' connect empty
    let completeAll exit = concat >> (fun p -> complete p exit)
    
    // Generalizations

    let private accept letter exit = State (letter, exit)

    let private loop body exit =
        let rec split = Split (enter, exit)
        and enter = complete body split
        split

    let private branch left right join = Split (complete left join, complete right join)

    // Implementation of control structures
    let makeChar            = ProtoAutom << accept << Letter
    let makeAnyChar ()      = ProtoAutom << accept <| Any
    let makeRange           = ProtoAutom << accept << Range << sortTuple
    let makeZeroOrMore      = ProtoAutom << loop
    let makeOneOrMore inner = ProtoAutom <| (complete inner << loop inner)
    let makeZeroOrOne       = ProtoAutom << branch empty
    let makeAlternation l r = ProtoAutom <| branch l r
