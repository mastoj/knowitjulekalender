#load "../helpers/Helpers.fsx"
open Helpers.WebHelper
open System

let fileContent = downloadAll "https://knowit-julekalender.s3.eu-central-1.amazonaws.com/2019-luke2/world.txt"
let lines = fileContent.Split([|'\n'|], StringSplitOptions.None)
let getCapacity (str: string) = 
    str.Trim()
    |> (fun s -> s.ToCharArray()) 
    |> Array.filter (fun c -> c = ' ')
    |> Array.length
let result = 
    lines 
    |> Array.map getCapacity 
    |> Array.sum
printfn "Result: %A" result
