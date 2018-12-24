open System
open System.Numerics

let letters = ['A' .. 'Z'] @ [ 'Ø'; 'Æ'; 'Å']

let input = "GODJULOGGODTNYTTÅR"

let reversedInput = input.ToCharArray() |> Array.rev

reversedInput 
|> Array.mapi (fun i c -> i, 1 + (letters |> List.findIndex (fun c' -> c = c')))
|> Array.fold (fun acc (index, charPos) -> 
    acc + BigInteger.Pow(BigInteger(letters.Length), index) * BigInteger(charPos)) (BigInteger(0))
