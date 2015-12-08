#load "Helpers.fsx"

open Math

let sieve = createPrimeSieve 1000
let sieveMap = sieve |> List.map (fun x -> x,x) |> Map.ofList

sieve
|> List.map (fun x -> x,(x |> reverse))
|> List.filter (fun (x,y) -> x <> y && sieveMap |> Map.containsKey y)
|> List.length