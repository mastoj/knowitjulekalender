#load "../helpers/Helpers.fsx"
open Helpers
open WebHelper

let getData() =
    let fileUrl = "https://s3-eu-west-1.amazonaws.com/knowit-julekalender-2018/input-palindrom.txt"
    downloadAll fileUrl |> String.split "," |> Array.map int64

let getSample() = [|4;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;4|] |> Array.map int64

let checkForPalindromeAt (input: int64 []) (startIndex: int) =
    let rec search startLow startHigh cnt acc accSum = seq {
        let startPos = startLow - cnt
        let endPos = startHigh + cnt
        if startPos < 0 || endPos >= input.Length || input.[startPos] <> input.[endPos]
        then ()
        else
            let acc = sprintf "%i%s%i" input.[startPos] acc input.[endPos]
            let accSum = (accSum + (input.[startPos] + input.[endPos]))
            yield acc, accSum
            yield! search startLow startHigh (cnt + 1) acc accSum
    }

    seq {
        yield! search startIndex startIndex 1 (sprintf "%i" input.[startIndex]) input.[startIndex]
        yield! search startIndex (startIndex + 1) 0 "" 0L
    }

let primeSieve = Math.createPrimeSieve 300000L |> List.map (fun n -> n,true) |> Map.ofList
let isPrime n =
    primeSieve |> Map.containsKey n

let data = getData()

#time
let result =
    data 
    |> Seq.mapi (fun i v -> i)
    |> Seq.filter (fun i -> i <> data.Length)
    |> Seq.map (checkForPalindromeAt data)
    |> Seq.collect id
    |> Seq.map snd
    |> Seq.filter isPrime
    |> Seq.max
result |> printfn "Result: %A"
data |> Array.sum
#time