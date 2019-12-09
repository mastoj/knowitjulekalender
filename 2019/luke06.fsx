#load "../.paket/load/netcoreapp3.1/System.Drawing.Common.fsx"
#load "../.paket/load/netcoreapp3.1/System.IO.FileSystem.fsx"


open System
open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Imaging
//open System.IO

let pngStream = new System.IO.FileStream("./2019/luke06.png", System.IO.FileMode.Open, System.IO.FileAccess.Read)
let image = new Bitmap(pngStream);

let x = 0 ^^^ 255


printfn "%A" x