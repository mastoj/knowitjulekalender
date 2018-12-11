#load "../helpers/Helpers.fsx"
open Helpers
open WebHelper

let fileUrl = "https://s3-eu-west-1.amazonaws.com/knowit-julekalender-2018/input-crisscross.txt"

let path = fileUrl |> downloadAll

let updateCoord (x,y) (step, direction) =
    match direction with
    | 'H' -> (x + (int (step.ToString())), y)
    | 'V' -> (x - (int (step.ToString())), y)
    | 'F' -> (x, y + (int (step.ToString())))
    | 'B' -> (x, y - (int (step.ToString())))
    | _ -> (x,y)

#time
let (x,y) = path.ToCharArray() |> Seq.ofArray |> Seq.pairwise |> Seq.fold updateCoord (0,0)
printfn "Result: [%i,%i]" x y
#time