#load "../helpers/Helpers.fsx"
open Helpers.WebHelper

let url = "https://julekalender.knowit.no/resources/2019-luke11/terreng.txt
"
//let runway = "IIGGFFAIISGIFFSGFFAAASS".ToCharArray() |> List.ofArray
let runway = (downloadAll url).Trim().ToCharArray() |> List.ofArray

type State = {
    Speed: int
    Distance: int
    IceChange: int
    IsUpHill: bool
}
let rec calculate runway state =
    if state.Speed <= 0 then state.Distance
    else
        let state' =
            match runway with
            | [] -> raise (exn "Not enough runway")
            | 'G'::_ ->
                {state with Speed = state.Speed - 27; IceChange = 0; IsUpHill = true}
            | 'I'::_ ->
                {state with Speed = (state.Speed + 12 + state.IceChange); IceChange = state.IceChange + 12; IsUpHill = true}
            | 'A'::_ ->
                {state with Speed = state.Speed - 59; IceChange = 0; IsUpHill = true}
            | 'S'::_ ->
                {state with Speed = state.Speed - 212; IceChange = 0; IsUpHill = true}
            | 'F'::_ ->
                let change = if state.IsUpHill then -70 else 35
                {state with Speed = state.Speed + change; IceChange = 0; IsUpHill = (not state.IsUpHill)}
        calculate (runway |> List.tail) { state' with Distance = state'.Distance+1 }

calculate runway { Speed = 10703437 ; Distance = 0; IceChange = 0; IsUpHill = true}


// G = gress. Senker farten til sleden med 27km/t
// I = is. Øker farten til sleden med 12km/t * antall isområder på rad
// A = asfalt. Senker farten til sleden med 59km/t
// S = skog. Senker farten til sleden med 212km/t
// F = fjell. Kommer alltid i par, der den første er oppover som senker farten med 70km/t mens den andre er nedoverbakke som øker farten med 35km/t.