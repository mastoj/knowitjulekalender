#load "../helpers/Helpers.fsx"
open Helpers
open WebHelper

let fileUrl = "https://s3-eu-west-1.amazonaws.com/knowit-julekalender-2018/input.spp"

let code = fileUrl |> downloadAll


type Token =
    | Space
    | Sum
    | Put3
    | Sum2
    | Subtract2
    | Multiply
    | Drop
    | Copy
    | Inc
    | Div
    | IsEven
    | KeepOdd
    | Max3

let parse (str: string) =
    let rec innerParse program searchingForNewLine (code) =
        match code with
        | [] -> program |> List.rev
        | x::rest when x = '\n' -> innerParse program false rest
        | x::rest when x = 'K' || searchingForNewLine -> innerParse program true rest
        | x::rest when searchingForNewLine |> not ->
            let token =
                match x with
                | ' ' -> Space
                | ':' -> Sum
                | '|' -> Put3
                | '\'' -> Sum2
                | '.' -> Subtract2
                | '_' -> Multiply
                | '/' -> Drop
                | 'i' -> Copy
                | '\\' -> Inc
                | '*' -> Div
                | '[' -> KeepOdd
                | ']' -> IsEven
                | '~' -> Max3
            innerParse (token::program) false rest
    innerParse [] false (str.ToCharArray() |> List.ofArray)

let execute program =
    let rec innerExecute stack program =
        match program, stack with
        | [], _ -> stack
        | Space::rest, _ -> innerExecute (31::stack) rest
        | Sum::rest, _ -> innerExecute [(stack |> List.sum)] rest
        | Put3::rest, _ -> innerExecute (3::stack) rest
        | Sum2::rest, a::b::stackRest -> innerExecute ((a+b)::stackRest) rest
        | Subtract2::rest, a::b::stackRest -> innerExecute ((b-a)::(a-b)::stackRest) rest
        | Multiply::rest, a::b::stackRest -> innerExecute (a::(a*b)::stackRest) rest
        | Drop::rest, _::stackRest -> innerExecute stackRest rest
        | Copy::rest, a::_ -> innerExecute (a::stack) rest
        | Inc::rest, a::stackRest -> innerExecute ((a+1)::stackRest) rest
        | Div::rest, a::b::stackRest -> innerExecute ((a/b)::stackRest) rest
        | IsEven::rest, a::stackRest -> 
            let stack = 
                if a%2 = 0 then 1::stackRest else stackRest
            innerExecute stack rest
        | KeepOdd::rest, a::stackRest ->
            let stack = 
                if a%2 = 1 then stack else stackRest
            innerExecute stack rest
        | Max3::rest, a::b::c::stackRest -> innerExecute ((max a (max b c))::stackRest) rest
    innerExecute [] program

code |> parse |> execute |> List.max |> printfn "Result: %A"