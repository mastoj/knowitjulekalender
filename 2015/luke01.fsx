#load "WebHelper.fsx"
open WebHelper
open System.Net
open System
open System.IO
open System.Text.RegularExpressions

"http://pastebin.com/raw.php?i=F8z0JWqa"       
|> download 
|> (fun (s:string) -> s.Split('\n'))
|> Array.filter (fun (s:string) -> Regex.IsMatch(s.Trim(), "^[a-z]{0,3}[0-9]{2,8}[A-Z]{3,}$"))
|> Seq.length
