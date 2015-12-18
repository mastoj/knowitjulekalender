open System
let input = [2907; 6165; 6129; 3468; 2040; 4331; 7935; 5683; 6004; 9694; 8092; 188; 5796; 1184; 8873; 3200; 1981; 9556; 9981; 1387; 7802; 8387; 9970; 7326; 5372; 28; 628; 3408; 6; 3425; 3071; 6021; 9989; 5077; 824; 938; 1399; 5607; 6973; 5703; 9609; 4398; 8247; 5164; 2026; 4; 4468; 9524; 8; 9227; 8969; 1746; 5593]
let test = [824;938;1399;5607;6973;5703;9609;4398;8247]
let test2 = [128;12]
let test3 = [3; 30; 34; 5; 9]

let repeat char times = 
    let rec repeat' times acc =
        if times = 0 then acc
        else repeat' (times-1) (char+acc)
    repeat' times ""

let addNormalization list = 
    let maxLength = list |> List.map (fun (s:string) -> s.Length) |> List.max
    list |> List.map (fun i -> i, (sprintf "%s%s" i (repeat "9" (maxLength-i.Length))))

let printLongestNumber list = 
    list
    |> List.map string 
    |> addNormalization
    |> List.sortByDescending snd
    |> List.map fst
    |> String.Concat
    |> printfn "%s"
    
[test3;test2;test;input] |> List.iter printLongestNumber