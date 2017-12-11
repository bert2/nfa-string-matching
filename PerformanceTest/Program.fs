open System
open System.Diagnostics
open System.IO
open FSharp.Charting
open MathNet.Numerics
open GlobMatcher
open Proc

type TestResult = {
    Commit: string
    Durations: (int * float) list
    PolynomialFits: (int * float) list
}

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

let debugBuild =
#if DEBUG
    true
#else
    false
#endif

let toCsvRecord {Commit = commit; PolynomialFits = fits; Durations = durations} =
    let toStr (l, r) = sprintf "%A; %A" l r
    let fits' = String.Join ("; ", fits |> List.map toStr)
    let durations' = String.Join ("; ", durations |> List.map toStr)
    sprintf "%s; %i; %s; %i; %s%s" commit fits.Length fits' durations.Length durations' Environment.NewLine

let fromCsvRecord (str:string) = 
    let parseTuples offset n (record:string[]) =
        [for i in 0..n-1 -> 
            let index = offset + i * 2
            let left = record.[index] |> int
            let right = record.[index + 1] |> float
            left, right]

    let record = str.Split ';'

    let fitsOffset = 2
    let numFits = record.[fitsOffset - 1] |> int
    let durationsOffset = 1 + 1 + numFits * 2 + 1
    let numDurations = record.[durationsOffset - 1] |> int

    {
        Commit = record.[0]
        PolynomialFits = parseTuples fitsOffset numFits record
        Durations = parseTuples durationsOffset numDurations record
    }

let fromCsv filename =
    File.ReadAllLines filename
    |> Array.toList
    |> List.map fromCsvRecord

let contains result results =
    results
    |> List.map (fun r -> r.Commit)
    |> List.contains result.Commit

let ensureCreated filename = 
    File.AppendAllText (filename, null)
    filename

let measurePerformance minPatternLen maxPatternLen repetitions =
    let commit = getCommitHash ()
    printfn "Performance testing commit %s" commit 

    if debugBuild then printfn "WARNING: running test on a DEBUG build"

    doWarmup 50

    printf "Running automaton with pattern of length 000 (00x)"
    let lengths = [minPatternLen..maxPatternLen]
    let durations = lengths |> List.map (delay measureTimeForPatternLength) |> List.map (repeat 5)
    printfn ""

    printf "Analyzing runtime behaviour..."
    let orders = [1..repetitions-1]
    let fits = orders |> List.map (fitPolynomial (toFloats lengths) (List.toArray durations))
    printfn "done"

    {
        Commit = commit
        Durations = List.zip lengths durations
        PolynomialFits = List.zip orders fits
    }

let toChart newResult oldResults = 
    {newResult with Commit = newResult.Commit + " (new)"}::oldResults
    |> List.map (fun r -> Chart.Line (r.Durations, Name = r.Commit, XTitle = "pattern length", YTitle = "runtime (ms)"))
    |> Chart.Combine
    |> Chart.WithLegend ()
    
let print result chart =
    printfn "Result data for commit %A already saved. Printing results to screen:" result.Commit
    printfn "\nGoodness of polynomial fits:\n%A" result.PolynomialFits
    printfn "\nMeasurements per pattern length:\n%A" result.Durations
    Chart.Show chart

let save csvFile result chart =
    printf "Saving results to %s..." csvFile
    let csv = toCsvRecord result
    File.AppendAllText (csvFile, csv)
    printfn "done"
    printf "Rendering performance results to perftest-results.png..."
    let imageFile = csvFile.Replace (".csv", ".png")
    Chart.Save imageFile chart
    printfn "done"

let doPerformanceTest minPatternLen maxPatternLen repetitions =
    let result = measurePerformance minPatternLen maxPatternLen repetitions
    let resultFile = "perftest-results.csv" |> ensureCreated
    let oldResults = fromCsv resultFile
    let chart = toChart result oldResults

    if oldResults |> contains result 
    then print result chart
    else save resultFile result chart

[<EntryPoint>]
let main args = 
    match args.Length with
    | 3 -> doPerformanceTest (int args.[0]) (int args.[1]) (int args.[2])
    | _ -> 
        printfn "Usage:   PerformanceTest.exe <min pattern length> <max pattern length> <number of repetitions>"
        printfn "Example: PerformanceTest.exe 1 100 5"

    if Debugger.IsAttached then Console.ReadKey (true) |> ignore
    0
