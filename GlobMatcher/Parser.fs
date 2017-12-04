namespace GlobMatcher

open Result

type Column = int

type InputState = {
    line: string
    column: Column
}

type Position = InputState

type Label = string

type ErrorMsg = string

type ParserError = ParserError of Label * ErrorMsg * Position

type Parser<'a> = {
    parse: InputState -> Result<'a * InputState, ParserError>
    label: Label
}

module Parser =
    open System

    let run parser input = parser.parse input

    let run' parser str = run parser {line = str; column = 0}

    let ret x = {
        label = "n/a"
        parse = fun input -> Success (x, input)
    }

    let bind f parser = {
        label = "n/a"
        parse = fun input ->
            match run parser input with
            | Failure err -> Failure err
            | Success (x, input') -> 
                let parser' = f x
                run parser' input'
    }

    let (>>=) x f = bind f x

    let map f = bind (f >> ret)

    let (<!>) = map

    let (|>>) parser f = map f parser

    let apply parserF parser = 
        parserF >>= (fun f -> 
        parser  >>= (fun x -> 
        ret (f x)))
    
    let (<*>) = apply

    let lift f parser = ret f <*> parser

    let lift2 f parser parser' = ret f <*> parser <*> parser'

    let sequence parsers =
        let prepend = (fun x xs -> x::xs) |> lift2
        List.foldBack prepend parsers (ret [])

    // Support functions
    let toString result = 
        match result with
        | Success (value, _) -> sprintf "%A" value
        | Failure (ParserError (lbl, err, pos)) -> 
            let formattedErr = sprintf "%*s^ %s" pos.column "" err
            sprintf "Error parsing %s:\n%s\n%s" lbl pos.line formattedErr
    
    let fail label errmsg input = Failure (ParserError (label, errmsg, input))

    let setLabel parser lbl = {
        label = lbl
        parse = fun input ->
            match parser.parse input with
            | Success s -> Success s
            | Failure err -> Failure err
    }

    let (<?>) = setLabel

    let nextChar input =
        let next = 
            if input.column >= input.line.Length 
            then None
            else Some input.line.[input.column]
        let input' = {input with column = input.column + 1}
        input', next

    let satisfy predicate label = {
        label = label
        parse = fun input ->
            match nextChar input with
            | _, None -> fail label "No more input" input
            | input', Some next ->
                if predicate next then 
                    Success (next, input')
                else 
                    fail label (sprintf "Unexpected '%c'." next) input
    }

    let pchar target = 
        let lbl = sprintf "'%c'" target
        satisfy ((=) target) lbl
    
    let andThen parser parser' =
        let lbl = sprintf "%s and then %s" (parser.label) (parser'.label)
        parser  >>= (fun result -> 
        parser' >>= (fun result' -> 
        ret (result, result')))
        <?> lbl

    let (.>>.) = andThen

    let (.>>) parser parser' = parser .>>. parser' |>> fst
      
    let (>>.) parser parser' = parser .>>. parser' |>> snd

    let orElse parser parser' = {
        label = "n/a"
        parse = fun input ->
            let lbl = sprintf "%s or %s" (parser.label) (parser'.label)
            match run parser input with
            | Success result -> Success result
            | Failure _ -> run (parser' <?> lbl) input
    }

    let (<|>) = orElse

    let choice parsers = Seq.reduce (<|>) parsers

    let anyOf chars = chars |> Seq.map pchar |> choice <?> sprintf "any of %A" chars

    let charsTo ctor = List.toArray >> System.String >> string >> ctor

    let pstring (str:string) = 
        str
        |> List.ofSeq 
        |> List.map pchar 
        |> sequence 
        |>> charsTo string
        <?> str

    let rec many parser =
        let rec zeroOrMore parser input =
            match run parser input with
            | Failure _ -> [], input
            | Success (x, input') -> 
                let (xs, input'') = zeroOrMore parser input'
                x::xs, input''
        {
            label = sprintf "zero or more %ss" parser.label
            parse = fun input -> Success (zeroOrMore parser input)
        }

    let many1 parser = 
        let lbl = sprintf "one or more %ss" parser.label
        parser      >>= (fun head ->
        many parser >>= (fun tail ->
        ret (head::tail)))
        <?> lbl

    let opt parser = Some <!> parser <|> ret None <?> sprintf "optional %s" parser.label

    let between p1 p2 p3 = p1 >>. p2 .>> p3
