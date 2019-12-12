open System
let numbers = [1000 .. 9999] |> List.filter (fun i -> i % 1111 > 0)

let sortNumbers n =
    let numArr = (sprintf "%04i" n).ToCharArray()
    let max = numArr |> Array.sort |> String |> int
    let min = numArr |> Array.sortDescending |> String |> int
    min, max

let solve n =
    let rec inner n count =
        if n = 6174 then count
        else
            let max, min = sortNumbers n
            inner (max - min) (count + 1)
    inner n 0

numbers
|> List.map solve
|> List.countBy id
