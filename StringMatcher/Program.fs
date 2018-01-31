open System
open System.Diagnostics
open Argu
open StringMatcher
open Result

type Args =
    | [<AltCommandLine("-g"); Unique>]
        Glob of string
    | [<AltCommandLine("-r"); Unique>]
        Regex of string
    | [<AltCommandLine("-t"); Unique>] 
        Text of string
    | [<AltCommandLine("-p")>] 
        PrintGraph
    | [<AltCommandLine("-i")>] 
        Interactive
    interface IArgParserTemplate with
        member this.Usage = 
            match this with
            | Regex _     -> "the regex pattern describing the strings to match (not combinable with '--glob')"
            | Glob _      -> "the glob pattern describing the strings to match (not combinable with '--regex')"
            | Text _      -> "the string to be matched"
            | PrintGraph  -> "prints the graph of the generated NFA as a link to https://gravizo.com/"
            | Interactive -> "starts interactive mode"

let argsParser = ArgumentParser.Create<Args> (errorHandler = ProcessExiter ())

let exit exitcode =
    if Debugger.IsAttached then Console.ReadKey(true) |> ignore
    exitcode

let printGravizoLink automaton =
    let dotscript = AutomPrinter.toDot automaton
    printfn "%s%s" "https://g.gravizo.com/svg?" (Uri.EscapeDataString dotscript)

let gatherUserInputs () =
    printf "Pattern: "
    let pattern = Console.ReadLine()
    printfn "Pattern uses glob syntax? Otherwise regex will be assumed. (y/n)"
    let parseF = if Console.ReadKey(true).KeyChar = 'y' then GlobParser.toAutomaton else RegexParser.toAutomaton
    printf "Text to match: "
    let text = Console.ReadLine()
    printf "Print graph of automaton? (y/n) "
    let print = Console.ReadKey(true).KeyChar = 'y'
    printfn ""
    (pattern, parseF, text, print)

let getInputs (args:ParseResults<Args>) =
    if args.Contains <@ Interactive @> then
        gatherUserInputs ()
    else
        let (pattern, parseF) =
            match args.TryGetResult <@ Glob @> with
            | Some p -> p, GlobParser.toAutomaton
            | _      -> args.GetResult <@ Regex @>, RegexParser.toAutomaton
        (pattern, parseF, args.GetResult <@ Text @>, args.Contains <@ PrintGraph @>)

[<EntryPoint>]
let main argv = 
    let args = argsParser.Parse (argv, raiseOnUsage = false)
    if args.IsUsageRequested then 
        argsParser.PrintUsage () |> printfn "%s"
        2
    elif not (args.Contains <@ Regex @>) && not (args.Contains <@ Glob @>) && not (args.Contains <@ Interactive @>) then
        printfn "ERROR:  missing parameter '--regex' or '--glob'."
        argsParser.PrintUsage () |> printfn "%s"
        2
    elif args.Contains <@ Regex @> && args.Contains <@ Glob @> then
        printfn "ERROR:  parameter '--regex' cannot be combined with '--glob'."
        argsParser.PrintUsage () |> printfn "%s"
        2
    else
        let (pattern, parseF, text, printGraph) = getInputs args
        match parseF pattern with
        | Failure msg -> 
            printfn "Incorrect pattern:\n%s" msg
            3
        | Success a ->
            if printGraph then printGravizoLink a
            let result = Autom.run a text
            printfn "Match: %A" result
            if result then 0 else 1
    |> exit
