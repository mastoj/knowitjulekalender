open System

let isLeapYear y = 
    y % 4 = 0 && (y % 100 <> 0 || y % 400 = 0)

let rec calculateStartDays acc yearsAndDays=
    match yearsAndDays, acc with
    | (y,days)::rest,[] -> 
        calculateStartDays ((y,days,1)::acc) rest
    | (y,days)::rest,(_,_,lastStartDay)::_ -> 
        calculateStartDays ((y,days,lastStartDay % 7 + 1)::acc) rest
    | [],_ -> acc

let yearsAndDays = 
    [1 .. 2015]
    |> List.map (fun i -> (i, isLeapYear i))
    |> List.map (fun (y,leapYear) -> (y, if leapYear then 366 else 365))
    |> calculateStartDays []
    |> List.filter (fun (_,_,i) -> i = 1)
    |> List.length
