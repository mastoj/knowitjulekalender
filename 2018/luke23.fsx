#load "../helpers/Helpers.fsx"
open Helpers
open System

module FNr =
    type Gender =
        | Male
        | Female

    type FNr = private {
        Text: string
        Date: DateTime
        Gender: Gender
        Numbers: int list
        IndividualNumber: int
        ControlNumber1: int
        ControlNumber2: int
    } with
        member this.gender = this.Gender
        member this.date = this.Date

    let parseDate (numbers: int list) =
        try
            let day = numbers.[0] * 10 + numbers.[1]
            let month = numbers.[2] * 10 + numbers.[3]
            let year = 1900 + numbers.[4] * 10 + numbers.[5]
            let date = DateTime(year, month, day)
            Some date
        with
        | _ -> 
            printfn "=> Invalid date: %A" numbers
            None

    let parseIndividualNumber (numbers: int list) =
        Some (numbers.[6] * 100 + numbers.[7] * 10 + numbers.[8])

    let parseNumbers (str: string) =
        let numbers = 
            str.ToCharArray() |> List.ofArray |> List.map (fun c -> c.ToString())
        let ns =
            numbers
            |> List.map int
        if ns.Length = 11 then Some ns
        else None

    let getGender individualNumber =
        if individualNumber % 2 = 0 then Female else Male

    let validateControlNumber multipliers (numbers: int list) =
        let product = multipliers |> List.mapi (fun index n -> numbers.[index] * n) |> List.sum
        let modProduct = product % 11
        if modProduct = 0 then Some 0
        elif modProduct = 1 then None
        else Some (11 - modProduct)
        |> Option.bind (fun n -> if n = numbers.[multipliers.Length] then Some n else None)

    let parse nr = 
        let text = nr
        let numbers = parseNumbers text
        let date = numbers |> Option.bind parseDate
        let individualNumber = numbers |> Option.bind parseIndividualNumber
        let gender = individualNumber |> Option.map getGender
        let controlNumber1 = numbers |> Option.bind (validateControlNumber [3;7;6;1;8;9;4;5;2])
        let controlNumber2 = numbers |> Option.bind (validateControlNumber [5;4;3;2;7;6;5;4;3;2])
        match numbers, date, individualNumber, gender, controlNumber1, controlNumber2 with
        | Some n, Some d, Some indN, Some g, Some c1, Some c2 ->
            {
                Text = text
                Date = d
                Gender = g
                Numbers = n
                IndividualNumber = indN
                ControlNumber1 = c1
                ControlNumber2 = c2
            } |> Some
        | _, _, _, _, _, _ -> None


open WebHelper

let fileUrl = "https://s3-eu-west-1.amazonaws.com/knowit-julekalender-2018/input-fnr.txt"
let data = download fileUrl |> List.ofSeq

open FNr

let bornInMonth m (fnr: FNr) = fnr.date.Month = m
let isOfGender gender (fnr: FNr) = fnr.gender = gender

data
|> List.map parse
|> List.choose id
|> List.filter (bornInMonth 8)
|> List.filter (isOfGender Female)
|> List.length
|> printfn "Result: %A"