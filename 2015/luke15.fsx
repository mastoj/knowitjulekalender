let grid = 
    [
        [34; 21; 32; 41; 25]
        [14; 42; 43; 14; 31]
        [54; 45; 52; 42; 23]
        [33; 15; 51; 31; 35]
        [21; 52; 33; 13; 23]
    ]
let startPos = 11

let numToCoord x =
    let col = x/10
    let row = x%10 
    col-1,row-1
    
let findTreasure startPos = 
    let rec findTreasure' pos path =
        let (row,col) = pos |> numToCoord 
        let pos' = grid.[row].[col]
        if pos' = pos then (pos::path) |> List.rev
        else findTreasure' pos' (pos::path)
    findTreasure' startPos []

findTreasure startPos |> List.map string |> String.concat "," |> printfn "%A"