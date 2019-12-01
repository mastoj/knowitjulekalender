#load "../helpers/Helpers.fsx"

open Helpers.WebHelper
open System

let fileContent = downloadAll "https://knowit-julekalender.s3.eu-central-1.amazonaws.com/sau.txt"
let example = "50, 52, 52, 49, 50, 47, 45, 43, 50, 55"

let split (delim: string) (s: string) = s.Split(delim) |> List.ofArray

let input =
    fileContent
    |> split ", "
    |> List.map (Int32.Parse)

type GameState = {
    Day: int
    Size: int
    Buffert: int
    NoHungryDays: int
}

type State =
    | TheEnd of int
    | InProgress of GameState

let update state sheeps =
    match state with
    | TheEnd x -> state
    | InProgress gs ->
        match gs.NoHungryDays, gs.Buffert + sheeps - gs.Size with
        | 5, _ -> TheEnd (gs.Day - 1)
        | _, x when x >= 0 -> 
            InProgress { gs with Day = gs.Day + 1; NoHungryDays = 0; Size = gs.Size + 1; Buffert = x}
        | _, x ->
            InProgress { gs with Day = gs.Day + 1; NoHungryDays = gs.NoHungryDays + 1; Size = gs.Size - 1; Buffert = 0}
#time
let result = input |> List.fold update (InProgress {Day = 0; Size = 50; Buffert = 0; NoHungryDays = 0})
#time
printfn "==> Result: %A" result