open System

let flipC = function
    | '6' -> '9'
    | '9' -> '6'
    | '0' | '1' | '8' as c -> c
    | x -> 'x'

let flip = string >> Seq.map flipC >> Seq.rev >> Seq.toArray >> String

[for x in 0 .. 100000 do if string x = flip x then yield x] |> List.length |> printfn "%A"