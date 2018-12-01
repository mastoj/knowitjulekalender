#load "WebHelper.fsx"
open WebHelper
open System

"http://pastebin.com/raw.php?i=sGbqMyCa"
|> downloadLines
|> List.map (Seq.sort >> Seq.toArray >> (fun cs -> String cs))
|> List.groupBy (fun w -> w)
|> Seq.filter (fun (k,ws) -> (ws |> List.length) > 1)
|> Seq.map (fun (k,v) -> v)
|> Seq.concat
|> Seq.length
