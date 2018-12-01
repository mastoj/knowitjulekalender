#load "WebHelper.fsx"
open WebHelper
open System

let numbers = 
    "http://pastebin.com/raw.php?i=sJVZp7BH"       
    |> download 
    |> (fun (s:string) -> s.Split('\n'))
    |> Array.map float
    |> List.ofArray

let rec calculateMaxDiff number rest ((start,stop,diff) as candidate) = 
    match rest with
    | [] -> candidate
    | next::rest' ->
        let (start',stop',diff') as candidate' = 
            rest
            |> List.map (fun i -> number,i,i-number)
            |> List.maxBy (fun (_,_,d) -> d)
        if diff' > diff 
        then calculateMaxDiff next rest' candidate' 
        else calculateMaxDiff next rest' candidate

calculateMaxDiff (List.head numbers) (List.tail numbers) (0.0,0.0,0.0)