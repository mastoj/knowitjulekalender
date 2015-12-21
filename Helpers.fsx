module Math
let divisible n x = x % n = 0

let reverse x = 
    x
    |> string 
    |> Seq.rev 
    |> Seq.toArray 
    |> (fun x -> new System.String(x))
    |> int

let createPrimeSieve n =
    let rec createPrimeSieve' candidates primes =
        match candidates with
        | [] -> primes
        | [x] -> x::primes
        | x::rest ->
            let primes' = x::primes
            let candidates' = 
                rest 
                |> List.filter (fun y -> y%x <> 0)
            createPrimeSieve' candidates' primes'
    createPrimeSieve' ([2 .. n]) []

module Roman =
    type RomanDigit = 
        | I | II | III | IIII 
        | IV | V 
        | IX | X | XX | XXX | XXXX  
        | XL | L 
        | XC | C | CC | CCC | CCCC 
        | CD | D 
        | CM | M | MM | MMM | MMMM
    type RomanNumeral = RomanNumeral of RomanDigit list 

    let digitToInt =
        function
        | I -> 1 | II -> 2 | III -> 3 | IIII -> 4 
        | IV -> 4 | V -> 5
        | IX -> 9 | X -> 10 | XX -> 20 | XXX -> 30 | XXXX -> 40 
        | XL -> 40 | L -> 50 
        | XC -> 90 | C -> 100 | CC -> 200 | CCC -> 300 | CCCC -> 400 
        | CD -> 400 | D -> 500 
        | CM -> 900 | M -> 1000 | MM -> 2000 | MMM -> 3000 | MMMM -> 4000

    let digitsToInt list = 
        list |> List.sumBy digitToInt 

    let toInt (RomanNumeral digits) = digitsToInt digits

    type ParsedChar = 
        | Digit of RomanDigit 
        | BadChar of char

    let rec toRomanDigitListRec charList = 
        match charList with
        // match the longest patterns first

        // 4 letter matches
        | 'I'::'I'::'I'::'I'::ns -> 
            Digit IIII :: (toRomanDigitListRec ns)
        | 'X'::'X'::'X'::'X'::ns -> 
            Digit XXXX :: (toRomanDigitListRec ns)
        | 'C'::'C'::'C'::'C'::ns -> 
            Digit CCCC :: (toRomanDigitListRec ns)
        | 'M'::'M'::'M'::'M'::ns -> 
            Digit MMMM :: (toRomanDigitListRec ns)

        // 3 letter matches
        | 'I'::'I'::'I'::ns -> 
            Digit III :: (toRomanDigitListRec ns)
        | 'X'::'X'::'X'::ns -> 
            Digit XXX :: (toRomanDigitListRec ns)
        | 'C'::'C'::'C'::ns -> 
            Digit CCC :: (toRomanDigitListRec ns)
        | 'M'::'M'::'M'::ns -> 
            Digit MMM :: (toRomanDigitListRec ns)

        // 2 letter matches
        | 'I'::'I'::ns -> 
            Digit II :: (toRomanDigitListRec ns)
        | 'X'::'X'::ns -> 
            Digit XX :: (toRomanDigitListRec ns)
        | 'C'::'C'::ns -> 
            Digit CC :: (toRomanDigitListRec ns)
        | 'M'::'M'::ns -> 
            Digit MM :: (toRomanDigitListRec ns)

        | 'I'::'V'::ns -> 
            Digit IV :: (toRomanDigitListRec ns)
        | 'I'::'X'::ns -> 
            Digit IX :: (toRomanDigitListRec ns)
        | 'X'::'L'::ns -> 
            Digit XL :: (toRomanDigitListRec ns)
        | 'X'::'C'::ns -> 
            Digit XC :: (toRomanDigitListRec ns)
        | 'C'::'D'::ns -> 
            Digit CD :: (toRomanDigitListRec ns)
        | 'C'::'M'::ns -> 
            Digit CM :: (toRomanDigitListRec ns)

        // 1 letter matches
        | 'I'::ns -> 
            Digit I :: (toRomanDigitListRec ns)
        | 'V'::ns -> 
            Digit V :: (toRomanDigitListRec ns)
        | 'X'::ns -> 
            Digit X :: (toRomanDigitListRec ns)
        | 'L'::ns -> 
            Digit L :: (toRomanDigitListRec ns)
        | 'C'::ns -> 
            Digit C :: (toRomanDigitListRec ns)
        | 'D'::ns -> 
            Digit D :: (toRomanDigitListRec ns)
        | 'M'::ns -> 
            Digit M :: (toRomanDigitListRec ns)

        // bad letter matches
        | badChar::ns -> 
            BadChar badChar :: (toRomanDigitListRec ns)

        // 0 letter matches
        | [] -> 
            []

    let toRomanDigitList (s:string) = 
        s.ToCharArray() 
        |> List.ofArray 
        |> toRomanDigitListRec

    /// Convert a string to a RomanNumeral
    /// Does not validate the input.E.g. "IVIV" would be valid
    let toRomanNumeral s = 
        let hasBadChar d =
            d |> List.exists (
                function
                | None -> true
                | _ -> false
            )
            
        let digits = 
            toRomanDigitList s
            |> List.map (
                function 
                | Digit digit -> 
                    Some digit 
                | BadChar ch -> 
                    None
                )
        if hasBadChar digits then None
        else digits |> List.map Option.get |> RomanNumeral |> Some

    let tryParse s = 
        match toRomanNumeral s with
        | Some rn -> toInt rn |> Some
        | None -> None

module Queue =
    open System.Collections.Generic
    let enqueue<'T> item (q:Queue<'T>) = 
        q.Enqueue(item)
        q
    
    let dequeue<'T> (q:Queue<'T>) = 
        if q.Count > 0 
        then
            let item = q.Dequeue()
            Some (item,q)
        else
            None
        
    let peek<'T> (q:Queue<'T>) = 
        q.Peek()
