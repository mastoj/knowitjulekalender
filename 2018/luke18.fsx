open System
#load "../helpers/Helpers.fsx"
open Helpers
open WebHelper

let getData() =
    let fileUrl = "https://s3-eu-west-1.amazonaws.com/knowit-julekalender-2018/input-rpslog.txt"
    downloadAll fileUrl

type Player = Alice | Bob | Charlie
type Move = Rock | Paper | Scissor
type State = {
    Players: Player list
    RemainingMoves: Move list
    Score: Map<Player, int>
}

module Move =
    let parse = function
        | 'S' -> Scissor
        | 'P' -> Paper
        | 'R' -> Rock

module Rules =
    let compareMoves = function
        | x, y when x = y -> None
        | Paper, Rock
        | Rock, Paper -> Some Paper
        | Scissor, Rock
        | Rock, Scissor -> Some Rock
        | Scissor, Paper
        | Paper, Scissor -> Some Scissor

module Game =
    let findWinners (playerMoves: (Player*Move) list) =
        let moves = playerMoves |> List.map snd
        let combos = moves @ [moves.[0]] |> List.pairwise
        let winningMoves =
            combos
            |> List.map Rules.compareMoves
            |> List.choose id
        if winningMoves |> List.isEmpty then playerMoves |> List.map fst
        else
            let winners =
                playerMoves
                |> List.map (fun (p, m) -> winningMoves |> List.contains m, p)
                |> List.filter fst
                |> List.map snd
            winners

    let play players moves =
        let initState = {
            Players = players
            RemainingMoves = moves
            Score = players |> List.map (fun p -> p, 0) |> Map.ofList
        }
        let rec innerPlay (remainingPlayers: Player list) state =
            let (state, remainingPlayers) =
                if remainingPlayers.Length = 1
                then
                    let newScore = state.Score.[remainingPlayers.[0]] + 1
                    let scores =  state.Score |> Map.add (remainingPlayers.[0]) (newScore)
                    { state with Score = scores}, state.Players
                else state, remainingPlayers
            match state.RemainingMoves with
            | [] -> state.Score
            | moves ->
                let nextMoves = state.RemainingMoves |> List.take remainingPlayers.Length
                let remainingMoves = state.RemainingMoves |> List.skip remainingPlayers.Length
                let state = { state with RemainingMoves = remainingMoves }
                let blah = List.zip remainingPlayers nextMoves
                let remainingPlayers = findWinners blah
                innerPlay remainingPlayers state
        innerPlay initState.Players initState

let sample = 
    "SSSSSRRPPSSSRPRPPSPPSSSSRPRRPRRPPRPPRRPPRPR".ToCharArray() 
    |> List.ofArray
    |> List.map Move.parse

let input =
    getData().ToCharArray()
    |> List.ofArray
    |> List.map Move.parse

let players = [Alice; Bob; Charlie]
Game.play players input
