
let countStep max =
    let rec countStep' = function
        | x when x = max -> 1
        | x when x+3<=max -> 
            countStep' (x+1) + countStep' (x+2) + countStep' (x+3)
        | x when x+2<=max -> 
            countStep' (x+1) + countStep' (x+2)
        | x -> 
            countStep' (x+1)
    countStep' 0

let res1 = countStep 30

let rec createSeq() = 
    let init = [0;1;2;4]
    let rec rest x y z =
        seq {
            let next = x + y + z
            yield next
            yield! rest next x y
        }
    seq {
        yield! init
        yield! rest 4 2 1
    }

let res2 = createSeq() |> Seq.skip 30 |> Seq.head