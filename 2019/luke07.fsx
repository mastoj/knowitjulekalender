open System

let divide x =
    let someCalc y' =
        let b = y' * x
        let rest = b % 27644437L
        if rest = 1L then Some y' else None
    let y =
        [2L .. 27644437L]
        |> List.map someCalc
        |> List.find (fun n -> n.IsSome)
        |> Option.get
    let z = 5897L * y
    let res' = z % 27644437L
    let rec makePositive n =
        if n >= 0L then n
        else makePositive (n+27644437L)
    
    makePositive res'


divide 7L
