
let max = 12345678987654321L
//let max = 1000L

let calculateOccurensesInInterval char start stop = 
    seq{ for i in start .. stop do yield i}
    |> Seq.map (string >> (fun s -> s |> Seq.filter ((=) char) |> Seq.length |> bigint))
    |> Seq.sum

let divideInIntervals end stepSize = 
    let divideInIntervals' start =
    seq {
        if 
    }
    
calculateOccurensesInInterval '2' 300001L 400000L
