
let rec generateParens n opened =
    match n,opened with
    | 0,0 -> 0
    | 0,x -> 
        1 
    | n,0 -> 
        generateParens (n-1) 1
    | n,x ->
        (generateParens (n) (x-1)) + 
        (generateParens (n-1) (x+1))

#time
generateParens 13 0
#time

let rec generateParens2 n opened acc =
    match n,opened with
    | 0,0 -> [acc]
    | 0,x -> 
        generateParens2 n (x-1) (acc+")")
    | n,0 -> 
        generateParens2 (n-1) 1 (acc+"(")
    | n,x ->
        (generateParens2 (n) (x-1) (acc+")")) @ 
        (generateParens2 (n-1) (x+1) (acc+"("))

#time
generateParens2 13 0 ""
#time