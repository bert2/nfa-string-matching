module ProtoAutom

open Xunit
open FsCheck
open StringMatcher
open System.Collections.Generic

let areEqualStates state state' =
    let visited = HashSet<_> ()

    let rec areEqualStates' state state' =
        if visited.Contains (state, state') then 
            true
        else
            match state, state' with
            | Final, Final -> true
            | State (l, next), State (l', next') when l = l' -> 
                visited.Add (state, state') |> ignore
                areEqualStates' next next'
            | Split (l, r), Split (l', r') -> 
                visited.Add (state, state') |> ignore
                areEqualStates' l l' && areEqualStates' r r'
            | _ -> false

    areEqualStates' state state'

let areEqual proto proto' =
    let a = ProtoAutom.complete proto Final
    let a' = ProtoAutom.complete proto' Final
    areEqualStates a a'

let rec protoAutom = 
    let leafProtos = [
        Gen.map ProtoAutom.makeChar    Arb.generate<char>
        Gen.map ProtoAutom.makeAnyChar (Gen.constant ())
        Gen.map ProtoAutom.makeRange   Arb.generate<char*char>]

    let rec protoAutomaton' size =
        if size <= 0 then
            Gen.oneof leafProtos
        else
            let subProto = protoAutomaton' (size - 1)
            let nestedProtos = [
                Gen.map  ProtoAutom.makeZeroOrMore  subProto
                Gen.map  ProtoAutom.makeOneOrMore   subProto
                Gen.map  ProtoAutom.makeZeroOrOne   subProto
                Gen.map2 ProtoAutom.makeAlternation subProto subProto]
            Gen.oneof <| leafProtos @ nestedProtos

    Gen.sized protoAutomaton'

[<Fact>]
let ``has identity element`` () =
    Check.VerboseThrowOnFailure (Prop.forAll 
        (Arb.fromGen protoAutom)
        (fun proto -> 
            let proto' = proto |> ProtoAutom.connect ProtoAutom.empty
            areEqual proto proto'))
