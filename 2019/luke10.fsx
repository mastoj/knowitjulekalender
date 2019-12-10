#load "../helpers/Helpers.fsx"
open Helpers.WebHelper
open System
open System.Globalization

type RowType =
    | Toothpaste of int
    | Schampoo of int
    | ToiletPaper of int

let url = "https://julekalender.knowit.no/resources/2019-luke10/logg.txt
"
let fileContent = downloadAll url
let lines = fileContent.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

let getValue (line: string) = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries).[1] |> int

let (|Toothpaste|_|) (line: string) =
    if line.EndsWith("tannkrem") then Some (Toothpaste (getValue line))
    else None

let (|Schampoo|_|) (line: string) =
    if line.EndsWith("sjampo") then Some (Schampoo (getValue line))
    else None

let (|ToiletPaper|_|) (line: string) =
    if line.EndsWith("toalettpapir") then Some (ToiletPaper (getValue line))
    else None

let (|LogDate|_|) (line: string) =
    let dateWithYear = "2018 " + line
    let (parseResult, value) = DateTime.TryParseExact(dateWithYear, "yyyy MMM dd:", null, DateTimeStyles.None)
    if parseResult then Some (LogDate value)
    else None


type ParseState = {
    CurrentDate: DateTime option
    Entries: (DateTime*RowType) list
}


let parse lines =
    let addEntry state entry =
        { state with Entries = (state.CurrentDate.Value, entry)::state.Entries }
    let rec inner lines state =
        match lines with
        | [] -> state
        | (LogDate d::rest) -> 
            inner rest{ state with CurrentDate = (Some d)}
        | (Toothpaste value)::rest -> 
            inner rest (addEntry state (Toothpaste value))
        | (Schampoo value)::rest -> 
            inner rest (addEntry state (Schampoo value))
        | (ToiletPaper value)::rest -> 
            inner rest (addEntry state (ToiletPaper value))
        | _::rest -> inner rest state
    inner lines { CurrentDate = None; Entries = []}

type AccState = {
    Toothpaste: int
    ToiletPaper: int
    Schampoo: int
    SundaySchampoo: int
    WednesdayToiletPaper: int
}
let initResultState = {
    Toothpaste = 0
    ToiletPaper = 0
    Schampoo = 0
    SundaySchampoo = 0
    WednesdayToiletPaper = 0
}

let accumulate state entry =
    let (date, what): DateTime*RowType = entry
    match what with
    | RowType.Toothpaste value ->
        { state with Toothpaste = state.Toothpaste + value }
    | RowType.ToiletPaper value ->
        let state' = { state with ToiletPaper = state.ToiletPaper + value }
        if date.DayOfWeek = DayOfWeek.Wednesday
        then { state' with WednesdayToiletPaper = state'.WednesdayToiletPaper + value }
        else state'
    | RowType.Schampoo value ->
        let state' = { state with Schampoo = state.Schampoo + value }
        if date.DayOfWeek = DayOfWeek.Sunday
        then { state' with SundaySchampoo = state'.SundaySchampoo + value }
        else state'

let calculateResult accState =
    let wholeToothPaste = accState.Toothpaste / 125
    let wholeSchampoo = accState.Schampoo / 300
    let wholeToiletPaper = accState.ToiletPaper / 25
    (wholeToothPaste * wholeSchampoo * wholeToiletPaper *
     accState.SundaySchampoo * accState.WednesdayToiletPaper)

lines
|> parse
|> (fun s -> s.Entries)
|> List.fold accumulate initResultState
|> calculateResult