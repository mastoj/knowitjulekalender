open System

module String =
    let split (split: string) (str: string) =
        str.Split([|split|], StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray

let configuration = """
Hjul 0: PLUSS101, OPP7, MINUS9, PLUSS101
Hjul 1: TREKK1FRAODDE, MINUS1, MINUS9, PLUSS1TILPAR
Hjul 2: PLUSS1TILPAR, PLUSS4, PLUSS101, MINUS9
Hjul 3: MINUS9, PLUSS101, TREKK1FRAODDE, MINUS1
Hjul 4: ROTERODDE, MINUS1, PLUSS4, ROTERALLE
Hjul 5: GANGEMSD, PLUSS4, MINUS9, STOPP
Hjul 6: MINUS1, PLUSS4, MINUS9, PLUSS101
Hjul 7: PLUSS1TILPAR, MINUS9, TREKK1FRAODDE, DELEMSD
Hjul 8: PLUSS101, REVERSERSIFFER, MINUS1, ROTERPAR
Hjul 9: PLUSS4, GANGEMSD, REVERSERSIFFER, MINUS9
"""
type Operation =
    | PLUSS4
    | PLUSS101
    | MINUS9
    | MINUS1
    | REVERSERSIFFER
    | OPP7
    | GANGEMSD
    | DELEMSD
    | PLUSS1TILPAR
    | TREKK1FRAODDE
    | ROTERPAR
    | ROTERODDE
    | ROTERALLE
    | STOPP

type Name = string
type Wheel = Operation list
type MachineState = Map<int, Wheel>
type Status = Running | Stopped
type GameState = {
    CurrentScore: int64
    MachineState: MachineState
    NextWheel: int
    Status: Status
}

let parseOperation (str: string ) =
    match str with
    | "PLUSS4" -> PLUSS4
    | "PLUSS101" -> PLUSS101
    | "MINUS9" -> MINUS9
    | "MINUS1" -> MINUS1
    | "REVERSERSIFFER" -> REVERSERSIFFER
    | "OPP7" -> OPP7
    | "GANGEMSD" -> GANGEMSD
    | "DELEMSD" -> DELEMSD
    | "PLUSS1TILPAR" -> PLUSS1TILPAR
    | "TREKK1FRAODDE" -> TREKK1FRAODDE
    | "ROTERPAR" -> ROTERPAR
    | "ROTERODDE" -> ROTERODDE
    | "ROTERALLE" -> ROTERALLE
    | "STOPP" -> STOPP
    | z -> raise (exn (sprintf "Invalid operation: %s" z))

let parseConfigLine (str: string) =
    let x = str |> String.split ":"
    let [_; operationsStr] = str |> String.split ":"

    operationsStr.Trim()
    |> String.split ", "
    |> List.map parseOperation

let parseConfiguration (str: string): MachineState =
    str 
    |> String.split "\n"
    |> List.mapi (fun index line -> index,parseConfigLine line)
    |> Map.ofList


let rotateWheel index (machineState: MachineState) : (Operation * MachineState) =
    let operations = machineState.[index]
    match operations with
    | operation::rest -> operation, (machineState |> Map.add index (rest @ [operation]))
    | _ -> sprintf "Invalid wheel: %A" machineState |> exn |> raise

let machine = configuration |> parseConfiguration

let initGame startWheel = {
    CurrentScore = startWheel
    MachineState = machine
    NextWheel = (startWheel % 10L) |> int
    Status = Running
}

let getOperation gameState =
    let (operation, machineState) = 
        gameState.MachineState
        |> rotateWheel gameState.NextWheel
    
    operation, { gameState with MachineState = machineState}

let update operation gameState =
//    printfn "==> Operation: %A " operation
    let add x gameState = { gameState with CurrentScore = gameState.CurrentScore + x}
    let rec addTo7 gameState =
        let numArr = gameState.CurrentScore.ToString().ToCharArray()
        if numArr.[numArr.Length - 1] = '7' then gameState
        else gameState |> add 1L |> addTo7
    
    let getMultiplier gameState =
        if gameState.CurrentScore < 0L then -1L else 1L

    let reverseNums gameState =
        let multiplier = getMultiplier gameState
        let score = gameState.CurrentScore * multiplier
        let reversedNum = score.ToString().ToCharArray() |> Array.rev |> String |> Int64.Parse
        { gameState with CurrentScore = reversedNum * multiplier }

    let getMsd gameState =
        let multiplier = getMultiplier gameState
        let score = gameState.CurrentScore * multiplier
        score.ToString() |> Seq.take 1 |> Array.ofSeq |> String |> Int64.Parse

    let gangeMsd gameState =
        let msd = getMsd gameState
        { gameState with CurrentScore = gameState.CurrentScore * msd }

    let deleMsd gameState =
        let msd = getMsd gameState |> float
        let score' = 
            (gameState.CurrentScore |> float)/msd 
            |> Math.Floor
            |> int64
        { gameState with CurrentScore = score' }

    let pluss1TilPar gameState =
        try
            let multiplier = getMultiplier gameState
            let score = gameState.CurrentScore * multiplier
            let score' = 
                score.ToString()
                |> Seq.map (fun c -> c.ToString() |> Int32.Parse)
                |> Seq.map (fun n -> if n % 2 = 0 then n + 1 else n)
                |> Seq.map (fun n -> n.ToString().ToCharArray().[0])
                |> Array.ofSeq
                |> String
            let score'' = 
                score'
                |> Int64.Parse
            { gameState with CurrentScore = score'' * multiplier }
        with
        | e -> printfn "State: %A" gameState; raise e

    let trekk1FraOdde gameState =
        let multiplier = getMultiplier gameState
        let score = gameState.CurrentScore * multiplier
        let score' = 
            score.ToString()
            |> Seq.map (fun c -> c.ToString() |> Int64.Parse)
            |> Seq.map (fun n -> if n % 2L = 0L then n else n - 1L)
            |> Seq.map (fun n -> n.ToString().ToCharArray().[0])
            |> Array.ofSeq
            |> String
            |> Int64.Parse
        { gameState with CurrentScore = score' * multiplier }

    let rotateWheels wheels gameState =
        let machineState =
            wheels
            |> List.fold (fun machineState wheel -> rotateWheel wheel machineState |> snd) gameState.MachineState
        { gameState with MachineState = machineState }

    let gameState =
        match operation with
        | PLUSS4 -> add 4L gameState
        | PLUSS101 -> add 101L gameState
        | MINUS9 -> add -9L gameState
        | MINUS1 -> add -1L gameState
        | REVERSERSIFFER -> reverseNums gameState
        | OPP7 -> addTo7 gameState
        | GANGEMSD -> gangeMsd gameState
        | DELEMSD -> deleMsd gameState
        | PLUSS1TILPAR -> pluss1TilPar gameState
        | TREKK1FRAODDE -> trekk1FraOdde gameState
        | ROTERPAR -> rotateWheels [0 .. 2 .. 8] gameState
        | ROTERODDE -> rotateWheels [1 .. 2 .. 9] gameState
        | ROTERALLE -> rotateWheels [0 .. 9] gameState
        | STOPP -> {gameState with Status = Stopped}
    let nextWheel =
        (gameState.CurrentScore % 10L) * (getMultiplier gameState) |> int

    { gameState with NextWheel = nextWheel}

let rec run iteration gameState =
//    printfn "Gamestate %i: %A" iteration gameState
    if gameState.Status = Stopped then gameState
    else
        let (operation, gameState') = getOperation gameState
        gameState' |> update operation |> run (iteration+1)

[ 1L .. 10L ]
|> List.map (initGame >> run 0)
|> List.map (fun g -> g.CurrentScore)
|> List.max
|> printfn "Result: %A"

module Tests =
    let test (operation, inputGameState, expectedGameState) =
        let actualGameState = update operation inputGameState
        if actualGameState <> expectedGameState
        then sprintf "==> Error: Operation: %A\n==> Expected: %A\nActual: %A" operation expectedGameState actualGameState |> Some
        else None

    
    let runTest() =
        let gs = initGame 2L
        let scenarios = [
            (PLUSS4, gs, {gs with CurrentScore = gs.CurrentScore + 4L; NextWheel = 6})
            (PLUSS101, gs, {gs with CurrentScore = gs.CurrentScore + 101L; NextWheel = 3})
            (MINUS9, gs, {gs with CurrentScore = gs.CurrentScore - 9L; NextWheel = 7})
            (MINUS1, gs, {gs with CurrentScore = gs.CurrentScore - 1L; NextWheel = 1})
            (REVERSERSIFFER, {gs with CurrentScore = 456L}, {gs with CurrentScore = 654L; NextWheel = 4})
            (REVERSERSIFFER, {gs with CurrentScore = -456L}, {gs with CurrentScore = -654L; NextWheel = 4})
            (OPP7, {gs with CurrentScore = 201L}, {gs with CurrentScore = 207L; NextWheel = 7})
            (OPP7, {gs with CurrentScore = -201L}, {gs with CurrentScore = -197L; NextWheel = 7})
        ]
        scenarios
        |> List.map test
        |> List.choose id

//Tests.runTest() |> List.iter (printfn "%A")
printfn "Done"