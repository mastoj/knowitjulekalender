let sample = [1; 2; 3; 4; 5; 6; 7]
let input = [1; 2; 3; 4; 5; 6; 7; 8; 7; 6; 5; 4; 3; 2; 1]

let mixAndMatch list =
    let rec internalFindX acc prev list =
        match list with
        | [] ->
            [
                ((prev)::acc |> List.rev)
                ((-prev)::acc |> List.rev)
            ]
        | x::rest ->
            match acc, prev with
            | _, 0 -> 
                internalFindX acc (prev*10+x) rest
            | [], _ ->
                internalFindX acc (prev*10+x) rest @
                internalFindX (prev::acc) x rest
            | _, _ ->
                internalFindX acc (prev*10+x) rest @
                internalFindX (prev::acc) x rest @
                internalFindX (-prev::acc) x rest
    internalFindX [] 0 list

#time
input 
|> mixAndMatch 
|> List.map (fun l -> l |> List.sum, l) 
|> List.filter (fun (s, l) -> s = 42)
|> List.length
#time