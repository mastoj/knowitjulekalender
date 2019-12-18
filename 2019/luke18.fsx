#load "../helpers/Helpers.fsx"
open Helpers.WebHelper
open System

module String =
    let split (c: string) (s: string) = s.Split(c, StringSplitOptions.RemoveEmptyEntries) |> List.ofArray


let url = "https://julekalender.knowit.no/resources/2019-luke18/names.txt"
let names = downloadAll url |> String.split "---"

let employeeUrl = "https://julekalender.knowit.no/resources/2019-luke18/employees.csv"
let employeesFile = 
    downloadAll employeeUrl
    |> String.split "\n"
    |> List.skip 1

let maleNames = names.[0] |> String.split "\n"
let femaleNames = names.[1] |> String.split "\n"
let lastName1 = names.[2] |> String.split "\n"
let lastNam2 = names.[3] |> String.split "\n"

let splitName (str: string) =
    let midPos = Math.Ceiling((str.Length |> float)/2.) |> int
    str.Substring(0, midPos), str.Substring(midPos)

let foldAscii op initialValue (str: string) = 
    str.ToCharArray()
    |> Array.map int64
    |> Array.fold op initialValue
let multiplyChars = foldAscii (*) 1L
let addChars = foldAscii (+) 0L

let alphabet = [ 'a' .. 'z' ] |> List.mapi (fun i c -> c, (i + 1)) |> Map.ofList
let alphabetPosition (c: char) =
    alphabet.[c]
let getAlphabetPositions (str: String) =
    str.ToLower().ToCharArray()
    |> List.ofArray 
    |> List.map alphabetPosition

let addAlphaLetters (str: string) =
    str |> getAlphabetPositions |> List.sum

let sortNumbers (n: int64) =
    let s =
        n.ToString().ToCharArray()
        |> Array.sortDescending
        |> String
    s |> Int64.Parse

type Gender = Male | Female
type Employee = {
    FirstName: string
    LastName: string
    Gender: Gender
}
let parseGender (s: string) = if s = "M" then Male else Female
let parseEmployeeRow (s: string) =
    let [firstName; lastName; gender] = String.split "," s
    {
        FirstName = (firstName.Replace(" ", ""))
        LastName = (lastName.Replace(" ", "").Replace("'", ""))
        Gender = (parseGender gender)
    }

let starWarsFirstName employee = 
    let firstNameNumber = employee.FirstName |> addChars |> int
    match employee.Gender with
    | Male -> maleNames.[firstNameNumber % maleNames.Length]
    | Female -> femaleNames.[firstNameNumber % femaleNames.Length]

let startWarsLastName employee =
    let (part1, part2) = splitName employee.LastName
    let num1 = (part1 |> addAlphaLetters) % lastName1.Length
    let num2 = part2 |> multiplyChars |> int64
    let num2' =
        match employee.Gender with
        | Male -> num2 * (employee.FirstName.Length |> int64)
        | Female -> num2 * ((employee.FirstName.Length + employee.FirstName.Length) |> int64)
    let num2'' = (num2' |> sortNumbers) % (lastNam2.Length |> int64) |> int
    sprintf "%s%s" (lastName1.[num1]) (lastNam2.[num2''])

let toStarWarsName employee =
    let firstName = starWarsFirstName employee
    let lastName = startWarsLastName employee
    sprintf "%s %s" firstName lastName

let solve = parseEmployeeRow >> toStarWarsName
"Jan,Johannsen,M" |> solve

employeesFile
|> List.map solve
|> List.groupBy id
|> List.sortByDescending (fun (_, xs) -> xs.Length)
|> List.map fst
|> List.head
