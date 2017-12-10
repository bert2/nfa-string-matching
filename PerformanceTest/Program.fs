open System
open System.Diagnostics
open MathNet.Numerics
open GlobMatcher

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

[<EntryPoint>]
let main argv = 
    doWarmup 50

    printf "Running automaton with pattern of length 000 (00x)"
    let lengths = [1..40]
    let durations = 
        lengths 
        |> List.map (delay measureTimeForPatternLength)
        |> List.map (repeat 5)
    printfn ""

    printf "Analyzing runtime behaviour..."
    let goodness = [1..4] |> List.map (fitPolynomial (toFloats lengths) (List.toArray durations))
    printfn "done"
    printfn "%A" goodness

    if Debugger.IsAttached then Console.ReadKey (true) |> ignore
    0
