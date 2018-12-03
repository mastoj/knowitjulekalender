#load "../helpers/Helpers.fsx"
open Helpers.WebHelper
open System.Text.RegularExpressions

let fileUrl = "https://s3-eu-west-1.amazonaws.com/knowit-julekalender-2018/input-rain.txt"

let data = download fileUrl |> List.ofSeq

let pattern = "\((\d+),(\d+)\);\((\d+),(\d+)\)"
let parseRow row =
    let matches = Regex.Match(row, pattern)
    let values = [ for i in 1 .. 4 do yield matches.Groups.[i].Value |> int ]
    ((values.[0], values.[1]),(values.[2],values.[3]))

let calculateK ((x1, y1),(x2, y2)) = float(y2 - y1)/float(x2 - x1)

let result =
    data
    |> Seq.map parseRow
    |> Seq.map (fun x -> x, x |> calculateK)
    |> Seq.groupBy snd
    |> Seq.map (fun (k, xs) -> xs |> Seq.length)
    |> Seq.max

printfn "Data: %A" result