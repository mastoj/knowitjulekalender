open System
let n = 100000

let flipC c =
    match c with
    | '6' -> '9'
    | '9' -> '6'
    | '0' | '1' | '8' -> c
    | _ -> 'x'

let flip =
    string >> Seq.map flipC >> Seq.rev >> Seq.toArray >> String

[0 .. n]
|> List.filter (fun x -> string x = flip x)
|> List.length
|> printfn "%A"
