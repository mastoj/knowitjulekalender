#load "Helpers.fsx"

open Math

let n = 10000
let sieve = createPrimeSieve n
let nonKnallSieve = sieve |> List.filter (fun i -> i<>2 && i<>3 && i<>5)
    
let isKnallTall n =
    if n = 1 then true
    else
        nonKnallSieve |> List.exists (fun x -> n%x=0) |> not

let res = 
    Seq.initInfinite (fun i -> i+1)
    |> Seq.filter isKnallTall
    |> Seq.skip (n-1)
    |> Seq.head
