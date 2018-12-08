#load "../helpers/Helpers.fsx"
open Helpers.WebHelper

let fileUrl = "https://s3-eu-west-1.amazonaws.com/knowit-julekalender-2018/input-dolls.txt"

let data = download fileUrl

let parse (str: string) =
    let parts = str.Split([|','|])
    parts.[0], (parts.[1] |> int)

let updateMap (map: Map<string, (int*int) list>) ((color, size): string*int) =
    let nonMatching = map |> Map.filter (fun k _ -> k <> color)
    let candidate =
        nonMatching
        |> Map.map (fun c (svs: (int*int) list) ->
            svs
            |> List.skipWhile (fun ((s, _): int*int) -> s <= size)
            |> List.head
        )
        |> Map.toList
        |> List.map snd
        |> List.sortWith (fun (s1, v1) (s2, v2) ->
            if v1 = v2 then s1 - s2
            else v1 - v2
        )
        |> List.rev
        |> List.head
    let newVal = 1 + (candidate |> snd)
    map |> Map.add color ((size, newVal)::(map.[color]))

let solve input =
    let defaultMapValue = [(System.Int32.MaxValue, 0)]
    let defaultMap = ["blue"; "green"; "red"] |> List.map (fun c -> c, defaultMapValue) |> Map.ofSeq
    input
    |> Seq.sortByDescending snd
    |> Seq.fold updateMap defaultMap

#time
data
|> Seq.map parse 
|> solve
|> Map.map (fun _ l -> l |> List.head)
|> Map.toSeq
|> Seq.map (snd >> snd)
|> Seq.max
|> printfn "Result: %A"
#time
