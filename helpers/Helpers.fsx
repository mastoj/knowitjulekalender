module String =
    open System
    let split (separator: string) (str: string) =
        str.Split([|separator|], StringSplitOptions.RemoveEmptyEntries)

module WebHelper =
    open System
    open System.IO
    open System.Net

    let downloadAll url =
        let req = WebRequest.Create(Uri(url))
        let resp = req.GetResponse()
        let stream = resp.GetResponseStream()
        let reader = new IO.StreamReader(stream)
        reader.ReadToEnd()

    let download url =
        let req = WebRequest.Create(Uri(url))
        let resp = req.GetResponse()
        let stream = resp.GetResponseStream()
        let reader = new IO.StreamReader(stream)
        let rec readSequence (reader: IO.StreamReader) = seq {
            let line = reader.ReadLine()
            if line |> isNull |> not
            then 
                yield line
                yield! readSequence reader
            else
                do reader.Close()
        }
        readSequence reader

    let downloadFloats = download >> Seq.map float

module Math =
    let createPrimeSieve n =
        let rec createPrimeSieve' candidates primes =
            match candidates with
            | [] -> primes
            | [x] -> x::primes
            | x::rest ->
                let primes' = x::primes 
                let candidates' = 
                    rest 
                    |> List.filter (fun y -> y%x <> 0L)
                createPrimeSieve' candidates' primes'
        createPrimeSieve' ([2L .. n]) []
