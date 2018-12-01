let chars = ['a' .. 'z']
let charsToValue = chars |> List.mapi (fun i c -> c,i) |> Map.ofSeq
let input = "evdhtiqgfyvcytohqppcmdbultbnzevdbakvkcdpbatbtjlmzaolfqfqjifkoanqcznmbqbeswglgrzfroswgxoritbw"
let toPalindromeCount (str:string) = 
    str
    |> Seq.zip (input |> Seq.rev)
    |> Seq.take (str.Length/2)
    |> Seq.sumBy (fun (c1,c2) -> System.Math.Abs(charsToValue.[c1] - charsToValue.[c2]))

input |> toPalindromeCount