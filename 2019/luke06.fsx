#load "../.paket/load/net463/System.Drawing.Common.fsx"

open System
open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Imaging
open System.IO

let pngStream = new FileStream("./2019/luke06.png", FileMode.Open, FileAccess.Read)
let image = new Bitmap(pngStream);

let x = 0 ^^^ 255


printfn "%A" x