#load "../helpers/Helpers.fsx"
open Helpers.WebHelper
open System

let url = "https://knowit-julekalender.s3.eu-central-1.amazonaws.com/2019-luke4/coords.csv
"
let fileContent = downloadAll url
let lines = fileContent.Split([|'\n'|], StringSplitOptions.None) |> List.ofArray |> List.skip 1 |> List.filter (fun s -> s <> "")

let toTuple (str: string) =
    try
        let [|item1; item2|] = str.Split([|','|])
        (item1 |> Int32.Parse, item2 |> Int32.Parse)
    with
    | e -> printfn "Error: %s" str; raise e

let input = lines |> List.map toTuple
let example = [(1, 3); (1, 0); (3, 2)]

type Position = int * int
type State = {
    Position: Position
    VisitedSquares: Map<Position, int>
    Time: int
    ReachedPosition: bool
}

let rec findTreasure coordinates state =
    let updateMap (position: Position) map =
        if map |> Map.containsKey position
        then map |> Map.add position (map.[position] + 1)
        else map |> Map.add position (1)

    let getTime position map =
        if map |> Map.containsKey position
        then map.[position] + 1
        else 1

    let updateState targetPosition state =
        let state = { state with ReachedPosition = false }
        let (xTarget,yTarget) = targetPosition
        let (x, y) = state.Position
        let state =
            match xTarget, yTarget, x, y with
            | xTarget, yTarget, x, y when xTarget = x && yTarget = y ->
                { state with ReachedPosition = true }
            | xTarget, _, x, y when xTarget > x ->
                { state with Position = (x+1, y)}
            | xTarget, _, x, y when xTarget < x ->
                { state with Position = (x-1, y)}
            | _, yTarget, x, y when yTarget > y ->
                { state with Position = (x, y+1)}
            | _, yTarget, x, y when yTarget < y ->
                { state with Position = (x, y-1)}
        if state.ReachedPosition 
        then state
        else
            let map = updateMap state.Position state.VisitedSquares
            let timeToAdd = getTime state.Position state.VisitedSquares
            { state with Time = state.Time + timeToAdd; VisitedSquares = map}


    match coordinates with
    | [] -> state
    | targetPosition::rest ->
        let state' = updateState targetPosition state
        if state'.ReachedPosition
        then findTreasure rest state'
        else findTreasure coordinates state'

let init = { Position = (0,0); VisitedSquares = [((0,0),1)] |> Map.ofList; Time = 0; ReachedPosition = false}
let result = findTreasure input init