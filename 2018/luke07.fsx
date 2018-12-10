#load "../helpers/Helpers.fsx"
open Helpers.WebHelper

let fileUrl = "https://s3-eu-west-1.amazonaws.com/knowit-julekalender-2018/input-vekksort.txt"

let data = download fileUrl
let numbers = data |> Seq.map int |> List.ofSeq


// implementing this https://en.wikipedia.org/wiki/Longest_increasing_subsequence
let solve numbers = numbers
    // let generateList (numbers: int list) ((l, pred, smallest): int*Map<int,int>*Map<int,int>): int list =
    //     let rec internalGenerate (numbers: int list) index k acc =
    //         if index < 0 then acc
    //         else
    //             let acc' = (numbers.[k])::acc
    //             let k' = pred.[k]
    //             internalGenerate numbers (index - 1) k' acc'
    //     internalGenerate numbers (l-1) (smallest.[l]) []

    // let rec internalSolve l (pred: Map<int, int>) (smallest: Map<int, int>) (numbers: int list) index =
    //     match numbers with
    //     | [] -> l, pred, smallest
    //     | currentValue::rest ->
    //         let newL = findLow currentValue 1 l smallest numbers
    //         let pred' = pred |> Map.add index (smallest |> Map.tryFind (newL - 1) |> Option.defaultValue 0)
    //         let smallest' = smallest |> Map.add newL index
    //         let l' = max newL l
    //         internalSolve l' pred' smallest' rest (index + 1)
    // and findLow currentValue low high smallest numbers =
    //     if low > high then low
    //     else
    //         let mid = (((low + high) |> float) / 2.) |> ceil |> int
    //         if numbers.[smallest.[mid]] < currentValue
    //         then findLow currentValue (mid+1) high smallest numbers
    //         else findLow currentValue mid (high-1) smallest numbers

    // internalSolve 0 Map.empty Map.empty numbers 0 |> generateList numbers


// 1 ; 1,1

// 1 ; 1,1
// 1, 1 ; 2,1

// 1 ; 1,1
// 1, 1 ; 2,1
// 1, 1, 7 ; 3,7

// 1 ; 1,1
// 1, 1 ; 2,1
// 1, 1, 7 ; 3,7
// 1, 1, 6 ; 3,6

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

let solve3 (input: int []) =
    let rec findLow currentIndex low high (input: int []) (smallestList: int []) =
        if low > high then low
        else
            let mid = ceil (float(low + high)/2.) |> int
            let (low, high) = 
                if input.[smallestList.[mid]] < input.[currentIndex] 
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

let example = [1; 1; 7; 5; 8; 9; 12; 11; 11; 13; 13; 12; 2; 3; 4; 4; 5; 6; 7; 8; 9; 10; 11; 12]
let exampleBest = [1; 1; 5; 4; 7; 8]


#time
let (predList, smallestList, l) =
    numbers
    |> Array.ofList
    |> solve3
let list =
    let res = Array.init l id
    let rec buildRes k index =
        if index = 0 then res
        else
            res.[index - 1] <- numbers.[k]
            let k = predList.[k]
            buildRes k (index-1)
    buildRes (smallestList.[l]) l
// // let result = numbers |> solve2
let length = list.Length
// printfn "%A" result
#time
// 1974