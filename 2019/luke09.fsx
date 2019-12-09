#load "../helpers/Helpers.fsx"
open Helpers.WebHelper
open System

let url = "https://julekalender.knowit.no/resources/2019-luke09/krampus.txt
"
let fileContent = downloadAll url
let numbers = 
    fileContent.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofArray
    |> List.map Int64.Parse

let isKrampustall x =
    let raised: int64 = x * x
    let splitNumber divider =
        let a = raised / divider
        let b = raised % divider
        if a <> 0L && b <> 0L then Some (a, b)
        else None
    let maxPowers = (raised.ToString().Length |> int64) 
    let dividers = 
        [1L .. maxPowers ]
        |> List.map (fun i -> 10.**(i |> float) |> int64)
    let candidates = 
        dividers
        |> List.map splitNumber 
        |> List.choose id
    candidates
    |> List.exists (fun (a,b) -> (a + b) = x)

numbers
|> List.filter isKrampustall
|> List.sum