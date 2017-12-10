module Performance

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
    let pattern, text = makeTestData "*" "a" n
    let automaton = GlobParser.toAutomaton' pattern
    measure (fun () -> Automaton.run automaton text)

let repeat n f =
    [0..n]
    |> List.map (fun _ -> f ())
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

let warmup = measureTimeForPatternLength 50
let lengths = [1..15]

let durations = 
    lengths 
    |> List.map (delay measureTimeForPatternLength)
    |> List.map (repeat 5)

let goodness = [1..4] |> List.map (fitPolynomial (toFloats lengths) (List.toArray durations))
