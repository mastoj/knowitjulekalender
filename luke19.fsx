
let countStep max =
    let rec countStep' = function
        | x when x = max -> 1
        | x when x+3<=max -> 
            countStep' (x+1) + countStep' (x+2) + countStep' (x+3)
        | x when x+2<=max -> 
            countStep' (x+1) + countStep' (x+2)
        | x -> 
            countStep' (x+1)
    countStep' 0

countStep 30