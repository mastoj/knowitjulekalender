#load "Helpers.fsx"

open Math

let n = 10000
// let sieve = createPrimeSieve (n*100)
// let nonKnallSieve = sieve |> List.filter (fun i -> i<>2 && i<>3 && i<>5) |> List.rev
//     
// let isKnallTall n =
//     if n = 1 then true
//     else
//         nonKnallSieve |> List.exists (fun x -> n%x=0) |> not
// 
// Seq.initInfinite (fun i -> i+1)
// |> Seq.filter isKnallTall
// |> Seq.skip (n-1)
// |> Seq.head
// |> printfn "%A"



let knallTallSeq() = 
    let rec knallTallSeq' candidate knallTall =
        seq {
            let div = knallTall |> List.tryFind (fun i -> candidate%i=0)
            match div with
            | Some x ->
                if knallTall |> List.exists (fun i -> candidate/x=i)
                then 
                    yield candidate
                    yield! knallTallSeq' (candidate+1) (candidate::knallTall)
                else yield! knallTallSeq' (candidate+1) knallTall
            | None -> yield! knallTallSeq' (candidate+1) knallTall
        }
    seq {
        yield 1
        yield 2
        yield 3
        yield 4
        yield 5
        yield! knallTallSeq' 6 [2;3;4;5]
    }
    
let res = knallTallSeq() |> Seq.skip (n-1) |> Seq.take 10 |> Seq.toList |> printfn "%A"
