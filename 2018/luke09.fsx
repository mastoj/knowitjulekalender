open Newtonsoft.Json
#load "../helpers/Helpers.fsx"
#load "../.paket/load/net45/Newtonsoft.Json.fsx"
open Newtonsoft.Json
open Helpers.WebHelper
open System.Security.Cryptography
open System.Text

let md5 (data : string) : string =
    use md5 = MD5.Create()
    let bytes = Encoding.UTF8.GetBytes(data)
    (StringBuilder(), md5.ComputeHash(bytes))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string
let key = "julekalender"
let fileUrl = "https://s3-eu-west-1.amazonaws.com/knowit-julekalender-2018/input-hashchain.json"

type Input = {
    Ch: string
    Hash: string
}

let deserialize (json:string) = JsonConvert.DeserializeObject<Input list>(json)

let getData = download >> String.concat "" >> deserialize

let solve key (input: Input list) =
    let rec internalSolve acc lastHash input =
        match input with
        | [] -> acc |> List.rev |> String.concat ""
        | _ ->
            let part1, part2 = input |> List.partition (fun i -> lastHash + i.Ch |> md5 = i.Hash)
            let (acc, lastHash) = part1 |> List.head |> (fun i -> i.Ch::acc, i.Hash)
            internalSolve acc lastHash part2
    internalSolve [] (key |> md5) input

let result = 
    getData fileUrl
    |> solve key

result |> printfn "Result: %A"