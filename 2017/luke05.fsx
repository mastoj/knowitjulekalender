open System.Numerics
let arr = [|1 .. 1000000|]

let solveOneNum num currentIndex (arr:int array) =
    let count = arr.[num - 1]
    let nextIndex = System.Math.Min(currentIndex + count - 1, arr.Length - 1)
    let indexes = [ currentIndex .. nextIndex ]
    indexes |> List.iter (fun i -> arr.[i] <- num)
    arr,nextIndex+1

let solve arr =
    let rec inner num currentIndex =
        let (arr: int array), index' = solveOneNum num currentIndex arr
        if index' >= arr.Length then arr
        else inner (num+1) index'
    inner 1 0

let res = solve arr |> Array.map BigInteger |> Array.sum
printfn "%A" res

