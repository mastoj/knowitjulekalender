open System.Collections.Generic
open System.Numerics

let n = 10000
let knallTallList() =
    [ for x in 0 .. 60 do
        for y in 0 .. 60 do
            for z in 0 .. 60 do
            yield 
                BigInteger.Pow((bigint 2), x) *
                BigInteger.Pow((bigint 3), y) *
                BigInteger.Pow((bigint 5), z)
    ] |> List.sort
    
#time
let res = knallTallList() |> List.skip 9999 |> List.head |> printfn "The winner is: %A"
#time

