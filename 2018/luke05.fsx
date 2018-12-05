let sample = [1; 2; 3; 4; 5; 6; 7]
let input = [1; 2; 3; 4; 5; 6; 7; 8; 7; 6; 5; 4; 3; 2; 1]

let mixAndMatch list =
    let rec internalFindX firstNumber acc prev list =
        match list with
        | [] ->
            [
                (prev)+acc
                (-prev)+acc
            ]
        | x::rest ->
            match firstNumber, prev with
            | _, 0 -> 
                internalFindX true acc (prev*10+x) rest
            | true, _ ->
                internalFindX true acc (prev*10+x) rest @
                internalFindX false (prev+acc) x rest
            | _, _ ->
                internalFindX false acc (prev*10+x) rest @
                internalFindX false (prev+acc) x rest @
                internalFindX false (-prev+acc) x rest
    internalFindX true 0 0 list

#time
input 
|> mixAndMatch 
|> List.filter (fun (s) -> s = 42)
|> List.length
#time