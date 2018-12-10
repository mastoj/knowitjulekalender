#load "../helpers/Helpers.fsx"
open Helpers.WebHelper

let fileUrl = "https://s3-eu-west-1.amazonaws.com/knowit-julekalender-2018/input-vekksort.txt"

let data = download fileUrl
let numbers = data |> Seq.map int |> List.ofSeq

let solve2 input =
    let rec internalSolve acc input =
        match input with
        | [] -> acc //|> List.sortByDescending fst |> List.head
        | x::rest when acc |> List.isEmpty -> internalSolve [x,1] rest
        | x::rest ->
            let (cnt, _) = 
                acc 
                |> List.skipWhile (fun (cnt', v') -> v' > x) 
                |> List.tryHead 
                |> Option.defaultValue (0, 0)
            let acc = (cnt+1, x)::acc |> List.sortBy snd |> List.sortByDescending fst
            internalSolve acc rest 
    internalSolve [] input |> List.maxBy fst |> fst

// implementing this https://en.wikipedia.org/wiki/Longest_increasing_subsequence
let solve (input: int []) =
    let rec findLow currentIndex low high (input: int []) (smallestList: int []) =
        if low > high then low
        else
            let mid = ceil (float(low + high)/2.) |> int
            let (low, high) = 
                if input.[smallestList.[mid]] <= input.[currentIndex] 
                then (mid+1, high) 
                else (low, mid-1)
            findLow currentIndex low high input smallestList

    let predList = Array.init (input.Length) (fun _ -> 0)
    let smallestList = Array.init (input.Length + 1) (fun _ -> 0)
    let rec internalSolve l currentIndex (predList: int []) (smallestList: int []) (input: int []) =
        if currentIndex = input.Length then (predList, smallestList, l)
        else
            let low = 1
            let high = l
            let lCandidate = findLow currentIndex low high input smallestList
            predList.[currentIndex] <- smallestList.[lCandidate - 1]
            smallestList.[lCandidate] <- currentIndex
            let l = if lCandidate > l then lCandidate else l
            internalSolve l (currentIndex + 1) predList smallestList input

    internalSolve 0 0 predList smallestList input

let buildList l (predList: int[]) (smallestList: int[]) (numbers: int list) = 
    let res = Array.init l id
    let rec buildRes k index =
        if index = 0 then res
        else
            res.[index - 1] <- numbers.[k]
            let k = predList.[k]
            buildRes k (index-1)
    buildRes (smallestList.[l]) l


#time
let (predList, smallestList, l) =
    numbers
    |> Array.ofList
    |> solve

printfn "Result: %i" l
#time
