//#r "packages\\FsCheck\\lib\\net45\\FsCheck.dll"
open System
//open FsCheck

let numbers = 
    [
        0, "pagh"
        1, "wa’"
        2, "cha’"
        3, "wej"
        4, "loS"
        5, "vagh"
        6, "jav"
        7, "Soch"
        8, "chorgh"
        9, "Hut"
    ] |> Map.ofList

let klingonNumbersToDec = 
    numbers |> Map.toSeq |> Seq.map (fun (k,v) -> v,k) |> Map.ofSeq

let tens =
    [
        10, "maH"
        100, "vatlh"
        1000, "SaD"
        10000, "netlh"
        100000, "bIp"
        1000000, "’uy’"
    ] |> Map.ofList

let klingonTensToDec = 
    tens |> Map.toSeq |> Seq.map (fun (k,v) -> v,k) |> Map.ofSeq

let decToKlingon dec = 
    if dec = 0 then numbers |> Map.find 0
    else
        let rec decToKlingon' rest div acc =
            match div with
            | 0 -> acc
            | 1 -> decToKlingon' 0 0 (sprintf "%s %s" acc (numbers |> Map.find rest))
            | _ ->
                let x = rest/div
                let y = rest%div
                match x,y with
                | 0,0 -> acc
                | 0,_ -> decToKlingon' y (div/10) acc
                | _,_ ->
                    let tenth = tens |> Map.find div
                    let num = numbers |> Map.find x
                    decToKlingon' y (div/10) (sprintf "%s %s%s" acc num tenth)
        decToKlingon' dec (1000000) "" |> (fun s -> s.Trim())

let klingonToDec klingon =
    let parse (word:string) =
        let keyOption = klingonTensToDec |> Map.tryFindKey (fun k v -> word.EndsWith(k))
        match keyOption with
        | None -> 
            klingonNumbersToDec |> Map.find word
        | Some k -> 
            let ten = klingonTensToDec |> Map.find k
            let num = klingonNumbersToDec |> Map.find (word.Replace(k, ""))
            ten * num

    let words = klingon |> (fun (s:string) -> s.Split([|' '|])) |> List.ofArray
    words
    |> List.map parse
    |> List.sum

let nums = 
    [
        9999999
        0
        4234134
        23
        23424    
    ] 
let klingonNums = nums |> List.map decToKlingon
let shouldBeTrue = nums = (klingonNums |> List.map klingonToDec)        

let convertTwiceIsSame n = 
    if n > 9999999 || n < 0 then true
    else n = (n |> decToKlingon |> klingonToDec)

//Check.One({Config.Quick with MaxTest = 50000}, convertTwiceIsSame) 

let twenty2 = 1 |> decToKlingon