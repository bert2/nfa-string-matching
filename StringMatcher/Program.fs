open System
open System.Diagnostics
open Argu
open StringMatcher
open Result

type Args =
    | [<AltCommandLine("-p")>][<Unique>][<Mandatory>]
        Pattern of string
    | [<AltCommandLine("-t")>][<Unique>][<Mandatory>] 
        Text of string
    | [<AltCommandLine("-g")>] 
        PrintGraph
    | [<AltCommandLine("-i")>] 
        Interactive
    interface IArgParserTemplate with
        member this.Usage: string = 
            match this with
            | Pattern _   -> "the pattern describing the strings to match"
            | Text _      -> "the string to be matched"
            | PrintGraph  -> "prints the graph of the generated NFA as a link to https://gravizo.com/."
            | Interactive -> "starts interactive mode"

let argsParser = ArgumentParser.Create<Args> ()

let exit exitcode =
    if Debugger.IsAttached then Console.ReadKey(true) |> ignore
    exitcode

let printGravizoLink automaton =
    let dotscript = AutomatonPrinter.toDot automaton
    printfn "%s%s" "https://g.gravizo.com/svg?" (Uri.EscapeDataString dotscript)

let gatherUserInputs () =
    printf "Pattern: "
    let pattern = Console.ReadLine()
    printf "Text: "
    let text = Console.ReadLine()
    printf "Print graph of automaton? (y/n) "
    let print = Console.ReadKey(true).KeyChar = 'y'
    printfn ""
    (pattern, text, print)

let getInputs (args:ParseResults<Args>) =
    if args.Contains <@ Interactive @> then
        gatherUserInputs ()
    else
        (args.GetResult <@ Pattern @>, args.GetResult <@ Text @>, args.Contains <@ PrintGraph @>)

[<EntryPoint>]
let main argv = 
    let args = argsParser.Parse (argv, raiseOnUsage = false)
    if args.IsUsageRequested then 
        argsParser.PrintUsage () |> printfn "%s"
        2
    else
        let (pattern, text, printGraph) = getInputs args
        match GlobParser.toAutomaton pattern with
        | Failure msg -> 
            printfn "Incorrect pattern:\n%s" msg
            3
        | Success a ->
            if printGraph then printGravizoLink a
            let result = Automaton.run a text
            printfn "Match: %A" result
            if result then 0 else 1
    |> exit
