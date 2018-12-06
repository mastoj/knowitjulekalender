#r "../packages/FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"
open FSharp.Collections.ParallelSeq

let isNulltungt (n: int64) =
    let strRep = sprintf "%i" n
    let length = strRep.Length |> float
    let numberOfZeros = strRep |> Seq.sumBy (fun c -> if c = '0' then 1 else 0) |> float
    numberOfZeros / length > 0.5

let recursiveSolve (n: int64) =
    let rec internalSolve acc n =
        if n = 0L then acc
        else
            if n |> isNulltungt then internalSolve (acc + n) (n-1L)
            else internalSolve acc (n-1L)
    internalSolve 0L n

let limit = 18163106

#time
Seq.init limit int64
|> Seq.map (fun i -> i, i |> isNulltungt)
|> Seq.filter snd
|> Seq.sumBy fst
|> printfn "Result: %A"
#time

#time
limit
|> int64
|> recursiveSolve
|> printfn "Result 2: %A"
#time

#time
Seq.init limit int64
|> PSeq.map (fun i -> i, i |> isNulltungt)
|> PSeq.filter snd
|> Seq.sumBy fst
|> printfn "Result 3: %A"
#time