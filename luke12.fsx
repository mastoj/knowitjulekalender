#time
seq {for x in 7L .. 7L .. 100000000L do if x%5L<>0L then yield x} |> Seq.sum
#time

#time
let rec doLuke12 current max acc =
    match current with
    | x when x > max -> acc
    | x -> 
        if x%5L <> 0L then doLuke12 (current+7L) max (acc+x)
        else doLuke12 (current+7L) max acc

let res = doLuke12 7L 100000000L 0L
#time   
