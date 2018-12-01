#load "WebHelper.fsx"
#load "Helpers.fsx"

open System.Collections.Generic
open WebHelper
open Math

let words = "http://pastebin.com/raw/LJX9cNvA" |> downloadLines
let start = "sand"
let stop = "hold"

type Result = {
    Word: string
    Nodes: string list
}

let getNeighbours word words = 
    let diffCount w1 w2 = 
        w1 |> Seq.zip w2 |> Seq.filter (fun (c1,c2) -> c1 <> c2) |> Seq.length 
    words 
    |> List.map (fun w -> w,diffCount w word)
    |> List.filter (snd >> (=) 1)
    |> List.map fst

let findPath start stop words = 
    let startResult = 
        {
            Word = start
            Nodes = []
        }
    let queue = new Queue<Result>()
    let rec findPath' (results:Queue<Result>) words = 
        match results |> Queue.dequeue with
        | None -> 
            failwith "No path"
        | Some (next, queue) ->
            if next.Word = stop then stop::next.Nodes |> List.rev
            else 
                let neighbours = words |> getNeighbours next.Word
                let words' = words |> List.except neighbours
                let queue' = 
                    neighbours |> 
                    List.fold (fun q w -> q |> Queue.enqueue {Word = w; Nodes = next.Word::next.Nodes}) queue
                findPath' queue' words'
    findPath' (queue |> Queue.enqueue startResult) words

#time
findPath start stop words |> (fun p -> printfn "Path: %A, Length: %i" p (p |> List.length))
#time

