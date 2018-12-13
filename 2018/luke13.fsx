#load "../helpers/Helpers.fsx"
open Helpers


let solve n1 n2 =
    let arr = Array.init 100000 (fun i -> if i <= (n1 + n2) then 1 else 0)
    let rec internalSolve i acc =
        seq {
            if arr.[i] = 1
            then 
                yield i
                do acc |> List.iter (fun x -> arr.[i+x] <- arr.[i+x] + 1)
                yield! internalSolve (i+1) (i::acc)
            else
                yield! internalSolve (i+1) (acc)
        }
    seq {
        yield n1
        yield n2
        yield! internalSolve (n2+n1) [n1; n2]
    }
#time
let primeSieve = Math.createPrimeSieve 10000L
let isPrime i =
    primeSieve |> List.contains i

solve 1 3
|> Seq.map int64 
|> Seq.filter isPrime
|> Seq.take 100 
|> Seq.sum 
|> (printfn "Result: %A")
#time