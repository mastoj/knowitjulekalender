#load "../helpers/Helpers.fsx"

open Helpers.Math



let solve numberOfFactors maxValue =
    let lowerBound = pown 2L numberOfFactors
    let maxPrime = maxValue / (lowerBound / 2L)
    let sieve = createPrimeSieve maxPrime |> List.rev
    let rec internalSolve currentDepth sieve acc =
        seq {
            if sieve = [] then ()
            else
                let prime = sieve |> Seq.head
                let acc' = prime * acc
                if currentDepth = numberOfFactors && acc' <= maxValue
                then
                    yield acc'
                if currentDepth < numberOfFactors && acc' < maxValue
                then
                    yield! (sieve |> Seq.map (fun p -> internalSolve (currentDepth + 1) (sieve |> List.filter (fun p' -> p' >= p)) acc') |> Seq.concat)
        }
    internalSolve 1 sieve 1L

let solve2 numberOfFactors maxValue =
    let lowerBound = pown 2L numberOfFactors
    let maxPrime = maxValue / (lowerBound / 2L)
    let sieve = createPrimeSieve maxPrime |> List.rev

    let rec internalSolve candidates currentValue remainingNumbers =
        if remainingNumbers = 0 && currentValue < maxValue
        then 1
        else
            match candidates with
            | (p::rest) ->
                let minMultiplier = pown p remainingNumbers
                if maxValue / minMultiplier < (currentValue)
                then 0
                else
                    internalSolve candidates (currentValue*p) (remainingNumbers - 1) +
                    internalSolve rest currentValue (remainingNumbers)
            | _ -> 0

    internalSolve sieve 1L numberOfFactors

let upperBound = 4294967296L
solve2 24 upperBound |> printfn "Juletall count: %A"
