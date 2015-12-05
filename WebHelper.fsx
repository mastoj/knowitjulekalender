module WebHelper
open System
open System.IO
open System.Net

let download url =
    let req = WebRequest.Create(Uri(url)) 
    use resp = req.GetResponse() 
    use stream = resp.GetResponseStream() 
    use reader = new IO.StreamReader(stream) 
    reader.ReadToEnd()
    
let downloadLines = 
    download 
    >> (fun (s:string) -> s.Split([|'\r';'\n'|]))
    >> List.ofArray
    >> List.filter (fun w -> w <> "")
    