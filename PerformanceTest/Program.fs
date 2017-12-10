open System
open System.Diagnostics
open MathNet.Numerics
open GlobMatcher
open Proc
open System.IO

let toFloats = List.map float >> List.toArray

let delay f x = fun () -> f x

let makeTestData wildcard word length =
    let text = String.replicate length word
    let pattern = (String.replicate length wildcard) + text
    pattern, text

let measure f = 
    let timer = Stopwatch.StartNew ()
    f () |> ignore
    timer.ElapsedMilliseconds

let measureTimeForPatternLength n = 
    printf "\b\b\b\b\b\b\b\b\b%03i (" n
    let pattern, text = makeTestData "*" "a" n
    let automaton = GlobParser.toAutomaton' pattern
    measure (fun () -> Automaton.run automaton text)

let repeat n f =
    [1..n]
    |> List.map (fun n -> 
        let v = f ()
        printf "%02ix)" n
        v)
    |> List.map float
    |> List.average

let polynomial n (ps:float[]) x = 
    [0..n] 
    |> List.map (fun order -> ps.[order] * Math.Pow (x, float order))
    |> List.sum

let fitPolynomial xs ys order =
    let ps = Fit.polynomial order xs ys
    let ys' = xs |> Array.map (polynomial order ps)
    GoodnessOfFit.RSquared (ys', ys)

let doWarmup n =
    printf "Warming up with pattern of length %i..." n
    let pattern, text = makeTestData "*" "a" n
    let automaton = GlobParser.toAutomaton' pattern
    Automaton.run automaton text |> ignore
    printfn "done"

let getCommitHash () =
    let exitCode, output = Proc.run "git" "rev-parse --short HEAD"
    match exitCode with
    | 0 -> output.Head
    | _ -> failwith (String.Join (Environment.NewLine, output))

let getBuildConfig () =
#if DEBUG
    "debug"
#else
    "release"
#endif

let toCsvRecord commit build fits durations =
    let toStr (l, r) = sprintf "%A; %A" l r
    let fits' = String.Join ("; ", fits |> List.map toStr)
    let durations' = String.Join ("; ", durations |> List.map toStr)
    sprintf "%s; %s; %s; %s%s" commit build fits' durations' Environment.NewLine

let hasResultsFor commitHash build resultCsv =
    if File.Exists resultCsv then 
        let resultId = sprintf "%s; %s" commitHash build
        File.ReadLines resultCsv 
        |> Seq.exists (fun l -> l.StartsWith resultId)
    else
        false

let doPerformanceTest minPatternLen maxPatternLen repetitions =
    let commit = getCommitHash ()
    let build = getBuildConfig ()
    printfn "Performance testing commit %s (%s build)" commit build

    doWarmup 50

    printf "Running automaton with pattern of length 000 (00x)"
    let lengths = [minPatternLen..maxPatternLen]
    let durations = lengths |> List.map (delay measureTimeForPatternLength) |> List.map (repeat 5)
    printfn ""

    printf "Analyzing runtime behaviour..."
    let orders = [1..repetitions-1]
    let fits = orders |> List.map (fitPolynomial (toFloats lengths) (List.toArray durations))
    printfn "done"

    let fits' = List.zip orders fits
    let durations' = List.zip lengths durations
    let resultFile = "perftest-results.csv"

    if resultFile |> hasResultsFor commit build then
        printfn "Result data for commit %A already saved. Printing results to screen:" commit
        printfn "\nGoodness of polynomial fits:\n%A" fits'
        printfn "\nMeasurements per pattern length:%A" durations'
    else
        printf "Saving results to %s..." resultFile
        let csv = toCsvRecord commit build fits' durations'
        File.AppendAllText (resultFile, csv)
        printfn "done"

[<EntryPoint>]
let main args = 
    match args.Length with
    | 3 -> doPerformanceTest (int args.[0]) (int args.[1]) (int args.[2])
    | _ -> 
        printfn "Usage:   PerformanceTest.exe <min pattern length> <max pattern length> <number of repetitions>"
        printfn "Example: PerformanceTest.exe 1 100 5"

    if Debugger.IsAttached then Console.ReadKey (true) |> ignore
    0
