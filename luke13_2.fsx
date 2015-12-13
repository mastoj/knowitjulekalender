open System.Collections.Generic
open System.Numerics

let n = 10000
let knallSeq() =
    let knalls = [(bigint 1)]
    let rec knallSeq' (two, three, five) (nums:BigInteger list) = 
        let numsLength = nums.Length
        let twoVal = nums.[numsLength - two - 1]*(bigint 2)
        let threeVal = nums.[numsLength - three - 1]*(bigint 3)
        let fiveVal = nums.[numsLength - five - 1]*(bigint 5)
        let next = [ twoVal; threeVal; fiveVal ] |> List.min
        let two' = if next = twoVal then two+1 else two
        let three' = if next = threeVal then three+1 else three
        let five' = if next = fiveVal then five+1 else five
        let nums' = next::nums
        seq {
            yield next
            yield! knallSeq' (two', three', five') nums'
        }
    seq {
        yield (bigint 1)
        yield! knallSeq' (0,0,0) knalls
    }
#time
let res = knallSeq() |> Seq.skip (n-1) |> Seq.head |> printfn "The winner is: %A"
#time
