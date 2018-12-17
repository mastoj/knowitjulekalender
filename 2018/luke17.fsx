open System

let getInput() = 
    [
        7.1,10.5
        18.8,9.2
        2.1,62.1
        74.2,1.5
        58.4,5.6
        15.9,6.2
        44.5,15.6
        88.1,53.4
        36.2,84.2
        26.9,8.5
    ]

let getSample() = 
    [
        (1.0,0.0); (3.0,0.0); (5.0,0.0)
    ]

let distance (node1) (node2) =
    let (x1,y1) = node1
    let (x2,y2) = node2
    sqrt ((x1-x2)**2. + (y1-y2)**2.)

let solve data : (float * int list) seq =
    let nodes = data |> List.mapi (fun i _ -> i)
    let distances =
        [ for i in 0 .. nodes.Length-1 do 
            for j in i + 1 .. nodes.Length-1 do
                yield 
                    [
                        ((i, j), distance data.[i] data.[j])
                        ((j, i), distance data.[i] data.[j])
                    ]
        ] |> List.collect id |> Map.ofList
    let rec fn path currentDistance currentNode remainingNodes = 
        seq {
            if remainingNodes |> List.isEmpty then yield (currentDistance, path |> List.rev)
            else
                yield! 
                    remainingNodes 
                    |> List.map (fun n -> fn (n::path) (currentDistance + distances.[n, currentNode]) n (remainingNodes |> List.filter (fun n' -> n' <> n)))
                |> Seq.collect id
        }
    [ for i in 0 .. data.Length - 1 
        do 
            yield fn [i] 0. i (nodes |> List.filter (fun n -> n <> i))
    ] |> Seq.collect id

getInput() |> solve |> List.ofSeq |> List.sortBy fst