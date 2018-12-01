#load "Helpers.fsx"
open Math
#time
seq {0 .. 7 .. 10000}
|> Seq.filter (reverse >> (divisible 7))
|> Seq.sum
#time

#time
[for x in 7 .. 7 .. 100000 do if x |> reverse |> (divisible 7) then yield x] |> List.sum
#time
