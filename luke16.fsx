
let max = 12345678987654321L
//let max = 1000L

let inline calculateOccurensesInInterval char start stop = 
    seq{ for i in start .. stop do yield i}
    |> Seq.map (string >> (fun s -> s |> Seq.filter ((=) char) |> Seq.length |> bigint))
    |> Seq.sum

// let divideInIntervals end stepSize = 
//     let divideInIntervals' start =
//     seq {
//         if 
//     }



let calculateForList = List.map (fun i -> i,calculateOccurensesInInterval '2' 0 (i))

let ones = [0 .. 1 .. 10] |> calculateForList
let tens = [0 .. 10 .. 100] |> calculateForList
let hundreds = [0 .. 100 .. 1000] |> calculateForList
// let thousands = [0 .. 1000 .. 10000] |> calculateForList
// let tensOfThousands = [0 .. 10000 .. 100000] |> calculateForList
// let hundredsOfThousands = [0 .. 100000 .. 1000000] |> calculateForList
// let millions = [0 .. 1000000 .. 10000000] |> calculateForList
// let tensOfMillions = [0 .. 10000000 .. 100000000] |> calculateForList
//calculateOccurensesInInterval '2' 0L 4000L
3895 |> calculateOccurensesInInterval '2' 0 

//f(1000) = (f(100)+10)*10
//f(5000) = (5*f(1000)+1000)




let splitInTenths n = 
    let rec splitInTenths rem tenth acc = 
        if rem = 0L then acc
        else 
            let rem' = rem/10L
            let x = rem - (rem%10L)*tenth
            let acc' = (tenth,(rem%10L),x)::acc
            let tenth' = tenth*10L
            splitInTenths rem' tenth' acc'
    splitInTenths n 1L []

let rec findTwos (tenth, n, rest) =
    match tenth,n with
    | 0L, _ -> 0L
    | 1L, x when x > 1L -> 1L
    | 1L, _ -> 0L
    | x, 2L -> 2L*findTwos(x,1L, rest)+1L
    | 10L, 1L -> 1L
    | x, 1L -> 
        (findTwos(x/10L,1L, rest)+x/100L)*10L  
    | x, y -> 
        y*findTwos(x,1L, rest)+x  

let alg = 27L |> splitInTenths |> List.map findTwos |> List.sum
//max |> splitInTenths |> List.map findTwos |> List.sum

let bf = calculateOccurensesInInterval '2' 0L 27L