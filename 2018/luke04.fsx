open System
open System.IO
open System.Drawing

let imageToBitmap (path: String) =
    new Bitmap(path)

let path = "./img/input-pokemon-jakt.png"

let bitmap = 
    path 
    |> imageToBitmap

let applyMask shift (color: Color) =
    let r = (color.R |> int) <<< shift |> byte |> int
    let b = (color.B |> int) <<< shift |> byte |> int
    let g = (color.G |> int) <<< shift |> byte |> int
    Color.FromArgb(color.A |> int, r, g, b)

let solveForMask shift =
    let outputPath = sprintf "./img/out_%i.png" shift
    let bitmap = imageToBitmap path
    
    for x in 0 .. (bitmap.Width - 1) do
        for y in 0 .. (bitmap.Height - 1 ) do
            bitmap.SetPixel(x, y, applyMask shift (bitmap.GetPixel(x, y))) 

    bitmap.Save(outputPath, Imaging.ImageFormat.Png)

[1 .. 8 ] |> List.map solveForMask

