#r "../packages/Newtonsoft.Json/lib/netstandard2.0/Newtonsoft.Json.dll"
open System
open System.IO
open Newtonsoft

type X = { a: string }
let x = Json.JsonConvert.DeserializeObject<X>("{a: 'tomas'}")

let file = File.ReadAllText("./2019/luke13.txt")
type Cell = {
    X: int
    Y: int
    Nord: bool
    Vest: bool
    Syd: bool
    Aust: bool
    Visited: bool
}

let cells = Json.JsonConvert.DeserializeObject<Cell list list>(file)
let maze = 
    cells
    |> List.concat
    |> List.map (fun c -> (c.X,c.Y) , c)
    |> Map.ofList

type Maze = Map<int*int, Cell>
type State = {
    CurrentPosition: int*int
    NumberOfRooms: int
    Maze: Maze
}

type Direction = 
    | Nord
    | Vest
    | Syd
    | Aust

let algo1 maze =
    let state = { CurrentPosition = 0,0; NumberOfRooms = 1; Maze = maze}
    
