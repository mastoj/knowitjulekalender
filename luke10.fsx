#load "WebHelper.fsx"
open WebHelper
open System
#time
let numbers = 
    "http://pastebin.com/raw.php?i=sJVZp7BH"       
    |> downloadFloats

let maxBuy numbers = 
    let revNums = numbers |> List.rev
    let rec maxBuy' maxPrice maxProfit rest acc = 
        match rest with
        | [] -> acc
        | x::rest' -> 
            let maxPrice' = if x > maxPrice then x else maxPrice
            let maxProfit' = if maxPrice' - x > maxProfit then maxPrice' - x else maxProfit
            maxBuy' maxPrice' maxProfit' rest' (maxProfit'::acc)
    maxBuy' (revNums |> List.head) 0.0 (revNums |> List.tail) [0.0]

let maxSell numbers = 
    let rec maxSell' minPrice maxProfit rest acc = 
        match rest with
        | [] -> 0.0 :: (acc |> List.rev)
        | x::rest' -> 
            let minPrice' = if x < minPrice then x else minPrice
            let maxProfit' = if x - minPrice' > maxProfit then x - minPrice' else maxProfit
            maxSell' minPrice' maxProfit' rest' (maxProfit'::acc)
    maxSell' (numbers |> List.head) 0.0 (numbers |> List.tail) []
        
let find2Max numbers = 
    let maxBuyList = maxBuy numbers
    let maxSellList = maxSell numbers
    List.zip maxBuyList maxSellList 
    |> List.map (fun (x,y) -> x+y)
    |> List.max 
    
let result = numbers |> find2Max
#time