module ProtoAutomMonoid

open Xunit
open FsCheck
open StringMatcher
open ProtoAutom
open Util

let areEqualStates = visitEach true (fun visitNext -> function
    | Final, Final -> true
    | State (l, next), State (l', next') when l = l' -> 
        visitNext (next, next')
    | Split (l, r), Split (l', r') -> 
        visitNext (l, l') && visitNext (r, r')
    | _ -> false)

let areEqual proto proto' =
    let a = complete proto Final
    let a' = complete proto' Final
    areEqualStates (a, a')

let rec protoAutom = 
    let leafProtos = [
        Gen.map makeChar    Arb.generate<char>
        Gen.map makeAnyChar (Gen.constant ())
        Gen.map makeRange   Arb.generate<char*char>]

    let rec protoAutomaton' size =
        if size <= 0 then
            Gen.oneof leafProtos
        else
            let subProto = protoAutomaton' (size - 1)
            let nestedProtos = [
                Gen.map  makeZeroOrMore  subProto
                Gen.map  makeOneOrMore   subProto
                Gen.map  makeZeroOrOne   subProto
                Gen.map2 makeAlternation subProto subProto]
            Gen.oneof <| leafProtos @ nestedProtos

    Gen.sized protoAutomaton'

let (===>) = connect

[<Fact>]
let ``has left identity`` () =
    Check.VerboseThrowOnFailure (Prop.forAll 
        (Arb.fromGen protoAutom)
        (fun proto -> 
            let proto' = empty ===> proto
            areEqual proto proto'))

[<Fact>]
let ``has right identity`` () =
    Check.VerboseThrowOnFailure (Prop.forAll 
        (Arb.fromGen protoAutom)
        (fun proto -> 
            let proto' = proto ===> empty
            areEqual proto proto'))

[<Fact>]
let ``associativity`` () =
    Check.VerboseThrowOnFailure (Prop.forAll 
        (Arb.fromGen <| Gen.three protoAutom)
        (fun (p1, p2, p3) -> 
            let proto  =  p1 ===> (p2  ===> p3)
            let proto' = (p1 ===>  p2) ===> p3
            areEqual proto proto'))
