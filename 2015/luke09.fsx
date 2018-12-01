open System
let letters = ['A' .. 'Z']
let letterLength = int64 letters.Length

let toExcelCol n = 
    let rec toExcelCol' n' acc =
        let rem = n'/letterLength
        let rest = n'%letterLength 
        if rem = 0L then 
            letters.[int rest]::acc
        else
            toExcelCol' (rem-1L) (letters.[int rest]::acc)
    toExcelCol' (n-1L) [] |> List.toArray |> String

[1L;2L;3L;26L;27L;28L;52L;79L;142453146368L]
|> List.map (fun i -> i, toExcelCol i)
