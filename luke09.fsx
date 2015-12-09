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

toExcelCol 142453146368L