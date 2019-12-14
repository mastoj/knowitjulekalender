open System

let alvesekvens (letters: int list) maxLength =
    let arr = Array.init maxLength (fun _ -> 0)
    let lettersLength = letters.Length
    let rec inner count index targetIndex =
        seq {
            match count, targetIndex - index with
            | x, 0 ->
                let letterIndex = (count + 1) % lettersLength
                let letter = letters.[letterIndex]
                arr.[index] <- letter
                yield arr.[index]
                yield! inner (count+1) (index + 1) (targetIndex + arr.[count + 1])
            | 0, _ ->
                arr.[index] <- letters.[0]
                yield letters.[0]
                yield! inner count (index + 1) targetIndex
            | x, y ->
                let letterIndex = (count) % lettersLength
                let letter = letters.[letterIndex]
                arr.[index] <- letter
                yield arr.[index]
                yield! inner count (index + 1) targetIndex
        }
    inner 0 0 (letters.[0])

#time
alvesekvens [2; 3; 5; 7; 11] 217532235 
|> Seq.take 217532235
|> Seq.sumBy (fun i -> if i = 7 then 7 else 0)
#time