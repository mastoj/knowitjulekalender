#load "../helpers/Helpers.fsx"
open Helpers
open WebHelper

let fileUrl = "https://s3-eu-west-1.amazonaws.com/knowit-julekalender-2018/input-bounding-crisscross.txt"

let path = fileUrl |> downloadAll

type State = {
    CurrentPosition: int*int
    VisitedPlaces: (int*int) list
    MaxX: int
    MinX: int
    MaxY: int
    MinY: int
}
let defaultState = {
    CurrentPosition = 0,0
    VisitedPlaces = [0,0]
    MaxX = 0
    MinX = 0
    MaxY = 0
    MinY = 0
}

let nextState state nextCoord =
    let (x,y) = state.CurrentPosition
    {
        CurrentPosition = nextCoord
        VisitedPlaces = (nextCoord::state.VisitedPlaces)
        MaxX = max state.MaxX x
        MinX = min state.MinX x
        MaxY = max state.MaxY y
        MinY = min state.MinY y
    }
let updateCoord state (step, direction) =
    let (x, y) = state.CurrentPosition 
    let stepSize = step.ToString() |> int
    match direction with
    | 'H' -> [1 .. (stepSize)] |> List.map (fun s -> (x+s,y)) |> List.fold nextState state
    | 'V' -> [1 .. (stepSize)] |> List.map (fun s -> (x-s,y)) |> List.fold nextState state
    | 'F' -> [1 .. (stepSize)] |> List.map (fun s -> (x,y+s)) |> List.fold nextState state
    | 'B' -> [1 .. (stepSize)] |> List.map (fun s -> (x,y-s)) |> List.fold nextState state

let calculateAnswer state =
    let uniqueCoordinates = state.VisitedPlaces |> List.distinct |> List.length |> decimal
    let totalNumberOfSquares = (state.MaxX - state.MinX + 1) * (state.MaxY - state.MinY + 1) |> decimal
    let ratio = uniqueCoordinates/(totalNumberOfSquares - uniqueCoordinates)
    System.Math.Round(ratio, 16)

#time
let endState = 
    path.ToCharArray()
    |> Seq.ofArray
    |> Seq.pairwise
    |> Seq.filter (fun (_, dir) -> ['H';'V';'F';'B'] |> List.contains dir)
    |> Seq.fold updateCoord defaultState
    |> calculateAnswer
printfn "Result: %A" endState
#time
