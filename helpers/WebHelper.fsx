module WebHelper
open System
open System.IO
open System.Net

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