module ProtoAutomaton

open Xunit
open FsCheck
open StringMatcher
open AutomatonBuilder

let rec protoAutomaton = 
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

[<Fact(Skip = "implement custom equality breaking circular dependencies first")>]
let ``has identity element`` () =
    Check.VerboseThrowOnFailure (Prop.forAll 
        (Arb.fromGen protoAutomaton)
        (fun proto -> 
            let proto' = connect proto empty

            resetIdGenerator ()
            let a = complete proto Final

            resetIdGenerator ()
            let a' = complete proto' Final

            a'.ToString() = a.ToString()))
