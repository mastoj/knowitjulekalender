open System

let repeat c n = 
    let rec repeat' n acc = 
        if n = 0 then acc
        else repeat' (n-1) (sprintf "%s%s" acc c)
    repeat' n ""
    
let normalize list = 
    let maxLength = list |> List.map (fun (s:string) -> s.Length) |> List.max
    list |> List.map (fun s -> s,(sprintf "%s%s" s (repeat (s.Substring(0,1))(maxLength - s.Length))))

let printLongestNumber list = 
    list
    |> List.map string
    |> normalize
    |> List.sortByDescending snd
    |> List.map fst
    |> String.concat ""
    |> printfn "%s"

let input = [2907; 6165; 6129; 3468; 2040; 4331; 7935; 5683; 6004; 9694; 8092; 188; 5796; 1184; 8873; 3200; 1981; 9556; 9981; 1387; 7802; 8387; 9970; 7326; 5372; 28; 628; 3408; 6; 3425; 3071; 6021; 9989; 5077; 824; 938; 1399; 5607; 6973; 5703; 9609; 4398; 8247; 5164; 2026; 4; 4468; 9524; 8; 9227; 8969; 1746; 5593]

input|> printLongestNumber
