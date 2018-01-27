module GeneratedRegexPatterns

open Xunit
open FsCheck
open StringMatcher

type Regex = 
    | Char   of char
    | Concat of Regex * Regex
    | Alt    of Regex * Regex
    | Group  of Regex
    | Star   of Regex
    | Plus   of Regex
    | Opt    of Regex

let rec genRegex () = 
    let leaves = [
        Gen.map Char Arb.generate<char>]

    let rec genRegex' size =
        if size <= 0 then
            Gen.oneof leaves
        else
            let subRegex = genRegex' (size - 1)
            let nested = [
                Gen.map Concat <| Gen.two subRegex
                Gen.map Alt    <| Gen.two subRegex
                Gen.map Group     subRegex
                Gen.map Star      subRegex
                Gen.map Plus      subRegex
                Gen.map Opt       subRegex]
            Gen.oneof <| leaves @ nested

    Gen.sized genRegex'

let rec genPattern = function
    | Char c        -> Gen.constant <| string c
    | Concat (l, r) -> Gen.map2 (sprintf "%s%s") (genPattern l) (genPattern r)
    | Alt (l, r)    -> Gen.map2 (sprintf "%s%s") (genPattern l) (genPattern r)
    | Group r       -> Gen.map  (sprintf "(%s)") (genPattern r)
    | Star r        -> Gen.map  (sprintf "%s*")  (genPattern r)
    | Plus r        -> Gen.map  (sprintf "%s+")  (genPattern r)
    | Opt r         -> Gen.map  (sprintf "%s?")  (genPattern r)

let rec genText = function
    | Char c        -> Gen.constant <| string c
    | Concat (l, r) -> Gen.map2 (+) (genText l) (genText r)
    | Alt (l, r)    -> Gen.oneof [genText l; genText r]
    | Group r       -> genText r
    | Star r        -> genText r |> Gen.listOf |> Gen.map (List.map string >> String.concat "")
    | Plus r        -> genText r |> Gen.nonEmptyListOf |> Gen.map (List.map string >> String.concat "")
    | Opt r         -> genText r |> Gen.optionOf |> Gen.map (function | Some s -> s | None -> "")

let matching = gen {
    let! r = genRegex ()
    let! p = genPattern r
    let! t = genText r
    return (p, t)
}

[<Fact>]
let ``test`` () =
    Check.VerboseThrowOnFailure (Prop.forAll 
        (Arb.fromGen matching)
        (fun (pattern, text) -> 
            let a = RegexParser.toAutomaton' pattern
            Autom.run a text))