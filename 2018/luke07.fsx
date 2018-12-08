#load "../helpers/Helpers.fsx"
open Helpers.WebHelper

let fileUrl = "https://s3-eu-west-1.amazonaws.com/knowit-julekalender-2018/input-vekksort.txt"

let data = download fileUrl
let numbers = data |> Seq.map int |> List.ofSeq


// implementing this https://en.wikipedia.org/wiki/Longest_increasing_subsequence
let solve numbers =
    let generateList (numbers: int list) ((l, pred, smallest): int*Map<int,int>*Map<int,int>): int list =
        let rec internalGenerate (numbers: int list) index k acc =
            if index < 0 then acc
            else
                let acc' = (numbers.[k])::acc
                let k' = pred.[k]
                internalGenerate numbers (index - 1) k' acc'
        internalGenerate numbers (l-1) (smallest.[l]) []

    let rec internalSolve l (pred: Map<int, int>) (smallest: Map<int, int>) (numbers: int list) index =
        match numbers with
        | [] -> l, pred, smallest
        | currentValue::rest ->
            let newL = findLow currentValue 1 l smallest numbers
            let pred' = pred |> Map.add index (smallest |> Map.tryFind (newL - 1) |> Option.defaultValue 0)
            let smallest' = smallest |> Map.add newL index
            let l' = max newL l
            internalSolve l' pred' smallest' rest (index + 1)
    and findLow currentValue low high smallest numbers =
        if low > high then low
        else
            let mid = (((low + high) |> float) / 2.) |> ceil |> int
            if numbers.[smallest.[mid]] < currentValue
            then findLow currentValue (mid+1) high smallest numbers
            else findLow currentValue mid (high-1) smallest numbers

    internalSolve 0 Map.empty Map.empty numbers 0 |> generateList numbers

#time
let result = numbers |> solve

printfn "%A" (result.Length, result)
#time
