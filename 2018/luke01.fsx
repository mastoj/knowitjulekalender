#load "../helpers/Helpers.fsx"
open Helpers.WebHelper

let fileUrl = "https://s3-eu-west-1.amazonaws.com/knowit-julekalender-2018/input-vekksort.txt"

let data = download fileUrl
let numbers = data |> Seq.map int64 |> List.ofSeq

let solve numbers =
    numbers 
    |> Seq.fold 
                (
                    fun (prev, acc) n -> 
                    if n < prev
                    then (prev, acc)
                    else (n, acc + n)
                ) (0L, 0L)
    |> snd

#time
let result = numbers |> solve

printfn "%A" result
#time