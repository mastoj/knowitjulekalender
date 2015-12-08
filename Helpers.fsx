module Math
let divisible n x = x % n = 0

let reverse x = 
    x
    |> string 
    |> Seq.rev 
    |> Seq.toArray 
    |> (fun x -> new System.String(x))
    |> int

let createPrimeSieve n =
    let rec createPrimeSieve' candidates primes =
        match candidates with
        | [] -> primes
        | [x] -> x::primes
        | x::rest ->
            let primes' = x::primes
            let candidates' = 
                rest 
                |> List.filter (fun y -> y%x <> 0)
            createPrimeSieve' candidates' primes'
    createPrimeSieve' ([2 .. n]) []
