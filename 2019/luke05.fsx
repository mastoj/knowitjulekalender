open System

let input = "tMlsioaplnKlflgiruKanliaebeLlkslikkpnerikTasatamkDpsdakeraBeIdaegptnuaKtmteorpuTaTtbtsesOHXxonibmksekaaoaKtrssegnveinRedlkkkroeekVtkekymmlooLnanoKtlstoepHrpeutdynfSneloietbol"

let chunk length (xs: seq<'T>) =
    let rec inner xs =
        seq {
            if xs |> Seq.isEmpty
            then ()
            else
                let elemToTake = min length (xs |> Seq.length)
                yield xs |> Seq.take elemToTake |> List.ofSeq
                yield! inner (xs |> Seq.skip elemToTake)
        }
    inner xs

let swapChunks xs =
    xs |> List.mapi (fun index e -> xs.[xs.Length - index - 1])

let revStep3 (str: string) =
    let chunks = str.ToCharArray() |> chunk 3 |> List.ofSeq
    let swapped = chunks |> swapChunks
    let flattened = swapped |> List.concat
    flattened

let revStep2 (str: char list) =
    let rec swap2 index =
        seq {
            if index >= str.Length / 2
            then ()
            else
                yield str.[index * 2 + 1]
                yield str.[index * 2]
                yield! swap2 (index + 1)
        }
    swap2 0 |> List.ofSeq

let revStep1 (str: char list) =
    [ str |> List.skip (str.Length / 2); str |> List.take (str.Length / 2) ]
    |> List.concat
    |> Array.ofList
    |> (fun c -> new string(c))

let log (x: 'T) = printfn "==> %A" x; x

let result =
    input
    |> revStep3
    |> log
    |> revStep2
    |> log
    |> revStep1


printfn "%s" result