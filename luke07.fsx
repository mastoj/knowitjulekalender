let divisible n x = x % n = 0 
let reverse x = 
    x
    |> string 
    |> Seq.rev 
    |> Seq.toArray 
    |> (fun (s) -> new System.String(s))
    |> int

seq {0 .. 7 .. 1000}
|> Seq.filter (reverse >> (divisible 7))
|> Seq.sum
