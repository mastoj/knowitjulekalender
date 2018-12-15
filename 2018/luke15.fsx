#load "../helpers/Helpers.fsx"
open Helpers
open WebHelper
open System.Text.RegularExpressions

let fileUrl = "https://s3-eu-west-1.amazonaws.com/knowit-julekalender-2018/input-gullbursdag.txt"

let data = download fileUrl |> List.ofSeq

let parse (str: string) =
    let pattern = "\\.(\\d+)"
    let matches = Regex.Matches(str, pattern)
    matches.[0].Groups.[1].Value |> int


let createGullYearList() =
    [0 .. 200] |> List.map (fun i -> i*i - i)

let gullYearList = createGullYearList()

let result =
    data 
    |> List.map parse
    |> List.map (fun y -> y, gullYearList |> List.contains y)
    |> List.filter snd
    |> List.length

printfn "Result: %A" result