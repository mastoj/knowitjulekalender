// Used for verification of smaller numbers
let inline calculateOccurensesInInterval char start stop = 
    seq{ for i in start .. stop do yield i}
    |> Seq.map (string >> (fun s -> s |> Seq.filter ((=) char) |> Seq.length |> bigint))
    |> Seq.sum

let splitInTenths n = 
    let rec splitInTenths rem tenth acc acc2' = 
        if rem = 0L then acc
        else 
            let rem' = rem/10L
            let acc' = (tenth,(rem%10L),acc2')::acc
            let tenth' = tenth*10L
            let acc2' = acc2' + tenth*(rem%10L)
            splitInTenths rem' tenth' acc' acc2'
    splitInTenths n 1L [] 0L

let rec findTwos (tenth, n, rest) =
    match tenth,n with
    | x, _ when x < 2L -> 0L
    | 1L, x when x > 1L -> 1L
    | 10L, 1L -> 1L
    | x, 2L -> 2L*findTwos(x,1L, rest)+rest+1L
    | x, 1L -> 
        (findTwos(x/10L,1L, rest)+x/100L)*10L  
    | x, y -> 
        y*findTwos(x,1L, rest)+x  
#time
12345678987654321L |> splitInTenths |> List.map findTwos |> List.sum |> printfn "%A"
#time
