#load "WebHelper.fsx"
#load "Helpers.fsx"

open WebHelper
open Math
open System

let numbers = 
    "http://pastebin.com/raw.php?i=p1eVAM5H"
    |> downloadLines

let (|Decimal|Roman|Binary|) (expr:string) = 
    if expr.StartsWith("0b") then Binary (expr, Convert.ToInt32(expr.Substring(2),2))
    else
        match Roman.tryParse expr with
        | Some v -> Roman (expr, v)
        | None ->
            let (result,v) = Int32.TryParse(expr)
            Decimal(expr,v)

let parse = function
    | Roman v -> v
    | Binary v -> v
    | Decimal v -> v

numbers
|> List.map parse
|> List.sortBy snd
|> List.skip (numbers.Length/2)
|> List.head
|> printfn "%A"