let input = [7096; 3; 3924; 2404; 4502; 4800; 74; 91; 9; 7; 9; 6790; 5; 59; 9; 48; 6345; 88; 73; 88; 956; 94; 665; 7; 797; 3978; 1; 3922; 511; 344;  6; 10; 743; 36; 9289; 7117; 1446; 10; 7466; 9; 223; 2; 6; 528; 37; 33; 1616; 619; 494; 48; 9; 5106; 144; 12; 12; 2; 759; 813; 5156; 9779; 969; 3; 257; 3; 4910; 65; 1; 907; 4464; 15; 8685; 54; 48; 762; 7952; 639; 3; 4; 8239; 4; 21; 306; 667; 1; 2; 90; 42; 6; 1; 3337; 6; 803; 3912; 85; 31; 30; 502; 876; 8686; 813; 880; 5309; 20; 27; 2523; 266; 101; 8; 3058; 7; 56; 6961; 46; 199; 866; 4; 184; 4; 9675; 92]

let stringValues = 
    input
    |> List.map (sprintf "%i")

let rec addX wantedLength (x: char) str =
    if wantedLength = 0 then str
    else addX (wantedLength - 1) x (sprintf "%s%c" str x)

let maxLength = stringValues |> List.maxBy (fun s -> s.Length) |> (fun s -> s.Length)
let sortedValues =
    stringValues
    |> List.map (fun s -> s, addX maxLength (s.[0]) s)
    |> List.sortByDescending snd
    |> List.map fst

sortedValues
|> String.concat ""
|> printfn "Result: %s"
