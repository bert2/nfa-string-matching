open System
open System.Diagnostics
open System.IO
open FSharp.Charting
open MathNet.Numerics
open GlobMatcher
open Proc
open System.Text.RegularExpressions

type TestResult = {
    Commit: string
    Durations: (int * float) list
    PolynomialFits: (int * float) list
}

[<Literal>]
let resultStore = "perftest-results.csv"

[<Literal>]
let resultChart = "perftest-results.png"

let toFloats = List.map float >> List.toArray

let delay f x = fun () -> f x

let makeTestData wildcard letter length =
    let text = String.replicate length letter
    let pattern = (String.replicate length wildcard) + text
    pattern, text

let measure f = 
    let timer = Stopwatch.StartNew ()
    f () |> ignore
    timer.ElapsedMilliseconds

let measureNfaTimeForPatternLength n = 
    printf "\b\b\b\b\b\b\b\b\b%03i (" n
    let pattern, text = makeTestData "*" "a" n
    let automaton = GlobParser.toAutomaton' pattern
    measure (fun () -> Automaton.run automaton text)

let measureRegexTimeForPatternLength n = 
    printf "\b\b\b\b\b\b\b\b\b%03i (" n
    let pattern, text = makeTestData ".*" "a" n
    let regex = Regex pattern
    measure (fun () -> regex.IsMatch text)

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

let parseCsv filename =
    File.ReadAllLines filename
    |> Array.toList
    |> List.map fromCsvRecord

let contains result results =
    results
    |> List.map (fun r -> r.Commit)
    |> List.contains result.Commit

let ensureCreated filename = File.AppendAllText (filename, null)

let measurePerformance f minPatternLen maxPatternLen repetitions getResultId =
    let commit = getResultId ()
    printfn "Performance testing commit %s" commit 

#if DEBUG
    printfn "WARNING: running test on DEBUG build"
#endif

    doWarmup 100

    printf "Running automaton with pattern of length 000 (00x)"
    let lengths = [minPatternLen..maxPatternLen]
    let durations = lengths |> List.map (delay f) |> List.map (repeat repetitions)
    printfn ""

    printf "Analyzing runtime behaviour..."
    let orders = [1..4]
    let fits = orders |> List.map (fitPolynomial (toFloats lengths) (List.toArray durations))
    printfn "done"

    {
        Commit = commit
        Durations = List.zip lengths durations
        PolynomialFits = List.zip orders fits
    }

let ensurePositiv (i, n) = (i, Math.Max (n, 1.))

let toChart = 
    List.map (fun r -> Chart.Line (r.Durations |> Seq.map ensurePositiv, Name = r.Commit))
    >> Chart.Combine
    >> Chart.WithXAxis (Title = "pattern length")
    >> Chart.WithYAxis (Title = "runtime (ms)", Log = true)
    >> Chart.WithLegend ()
    
let print result chart =
    printfn "Result data for commit %A already saved. Printing results to screen:" result.Commit
    printfn "\nGoodness of polynomial fits:\n%A" result.PolynomialFits
    printfn "\nMeasurements per pattern length:\n%A" result.Durations
    Chart.Show chart

let save result chart =
    printf "Saving results to %s..." resultStore
    let csv = toCsvRecord result
    File.AppendAllText (resultStore, csv)
    printfn "done"
    printf "Rendering performance results to %s..." resultChart
    Chart.Save resultChart chart
    printfn "done"

let render chartF = parseCsv resultStore |> toChart |> chartF

let doPerformanceTest f minPatternLen maxPatternLen repetitions getResultId =
    let result = measurePerformance f minPatternLen maxPatternLen repetitions getResultId
    ensureCreated resultStore
    let oldResults = parseCsv resultStore
    let allResults = oldResults@[result]
    let chart = toChart allResults

    if oldResults |> contains result 
    then print result chart
    else save result chart

[<EntryPoint>]
let main argv = 
    let args = argv |> Array.map (fun s -> s.ToLower())
    match args.Length with
    | 1 when args.[0] = "--rendertofile"    -> render (Chart.Save resultChart)
    | 1 when args.[0] = "--rendertoscreen"  -> render (Chart.Show)
    | 1 when args.[0] = "--testcsharpregex" -> doPerformanceTest measureRegexTimeForPatternLength 1 18 1 (fun () -> "C# Regex class")
    | 3 -> doPerformanceTest measureNfaTimeForPatternLength (int args.[0]) (int args.[1]) (int args.[2]) getCommitHash
    | _ -> 
        printfn "Usage:   PerformanceTest.exe <min pattern length> <max pattern length> <number of repetitions>"
        printfn "         PerformanceTest.exe --renderToFile"
        printfn "         PerformanceTest.exe --renderToScreen"
        printfn "         PerformanceTest.exe --testCSharpRegex"
        printfn "Example: PerformanceTest.exe 1 100 5"

    if Debugger.IsAttached then Console.ReadKey (true) |> ignore
    0
