module Performance

open System
open System.Diagnostics
open MathNet.Numerics
open GlobMatcher
open System.Text.RegularExpressions

let toFloats = List.map float >> List.toArray

let measure f = 
    let timer = Stopwatch.StartNew ()
    f () |> ignore
    timer.ElapsedMilliseconds

let derive = List.pairwise >> List.map (fun (y1:float, y2:float) -> y2 - y1)

let polynomial n (ps:float[]) x = 
    [0..n] 
    |> List.map (fun order -> ps.[order] * Math.Pow (x, float order))
    |> List.sum

let fitPolynomial xs ys order =
    let ps = Fit.polynomial order xs ys
    let ys' = xs |> Array.map (polynomial order ps)
    GoodnessOfFit.RSquared (ys', ys)

let window3 xs =
    xs
    |> List.pairwise 
    |> List.pairwise 
    |> List.map (fun ((l, m), (_, r)) -> (l, m, r))

let filter3 f xs =
    let first = List.head xs
    let last = List.last xs
    let result = xs |> window3 |> List.map f
    first::result@[last]

let median3 (l, m, r) = 
    [l; m; r]
    |> List.sort 
    |> List.item 1

let smooth3 (l, m, r) =
    if l <= m && m <= r
    then m 
    else (l + r) / 2.0
    
let makeText n = String.replicate n "a"

let makePattern wildcard n = (String.replicate n wildcard) + (makeText n)

let lazy' f x = fun () -> f x

let measureTimeForPatternLength n = 
    let pattern = makePattern "*" n
    let text = makeText n
    let automaton = GlobParser.toAutomaton' pattern
    measure (fun () -> Automaton.run automaton text)

let measureTimeForPatternLength' n = 
    let pattern = makePattern ".*" n
    let text = makeText n
    measure (fun () -> Regex.IsMatch (text, pattern))

let repeat n f =
    [0..n]
    |> List.map (fun _ -> f ())
    |> List.map float
    |> List.average

let warmup = measureTimeForPatternLength 50
let lengths = [1..15]

let durations = 
    lengths 
    |> List.map (lazy' measureTimeForPatternLength)
    |> List.map (repeat 5)

let durations' = 
    lengths 
    |> List.map (lazy' measureTimeForPatternLength')
    |> List.map (repeat 5)

let goodness = [1..4] |> List.map (fitPolynomial (toFloats lengths) (List.toArray durations))
let goodness' = [1..4] |> List.map (fitPolynomial (toFloats lengths) (List.toArray durations'))
