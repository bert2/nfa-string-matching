module ProtoAutom

open Xunit
open FsCheck
open StringMatcher

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

[<Fact(Skip = "implement custom equality breaking circular dependencies first")>]
let ``has identity element`` () =
    Check.VerboseThrowOnFailure (Prop.forAll 
        (Arb.fromGen protoAutom)
        (fun proto -> 
            let proto' = ProtoAutom.connect proto ProtoAutom.empty

            let a = ProtoAutom.complete proto Final
            let a' = ProtoAutom.complete proto' Final

            a'.ToString() = a.ToString()))
