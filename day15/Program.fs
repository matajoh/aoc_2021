open System
open System.IO
open System.Collections.Generic


let tryGet array (row, col) =
    let rows = Array2D.length1 array
    let cols = Array2D.length2 array
    match row, col with
    | (row, _) when row < 0 -> None
    | (row, _) when row >= rows -> None
    | (_, col) when col < 0 -> None
    | (_, col) when col >= cols -> None
    | (row, col) -> Some (Array2D.get array row col)


let neighbors (row, col) = [row - 1, col; row + 1, col; row, col + 1; row, col - 1]


let enumeratePoints array =
    let rows = Array2D.length1 array
    let cols = Array2D.length2 array
    [0 .. rows - 1]
    |> List.collect (fun row -> 
                        [0 .. cols - 1]
                        |> List.map (fun col -> row, col))


let getScore score point =
    let maybeScore = Map.tryFind point score
    match maybeScore with
    | Some score -> score
    | None -> Int32.MaxValue


type AStar = {
    Risk: int[,];
    GScore : Map<int * int, int>;
    OpenSet : Set<int * int>;
    H : (int * int) -> int;
    Queue : PriorityQueue<int * int, int>
    Goal : int * int
}


let updateAStar astar current neighbor score =
    let gScore = Map.change neighbor (fun _ -> Some score) astar.GScore
    let openSet = 
        if not (Set.contains neighbor astar.OpenSet) then
            astar.Queue.Enqueue(neighbor, score + astar.H neighbor)
            Set.add neighbor astar.OpenSet
        else
            astar.OpenSet

    { astar with GScore=gScore; OpenSet=openSet}


let rec addNeighbors astar current neighbors =
    match neighbors with
    | [] -> astar
    | neighbor :: tail ->
        match tryGet astar.Risk neighbor with
        | Some distance ->
            let tentativeGScore = (Map.find current astar.GScore) + distance
            let gScore = getScore astar.GScore neighbor
            if tentativeGScore < gScore then
                let newAStar = updateAStar astar current neighbor tentativeGScore
                addNeighbors newAStar current tail
            else
                addNeighbors astar current tail
        | None -> addNeighbors astar current tail
                

let rec findMinPath astar =
    let current = astar.Queue.Dequeue()
    if current = astar.Goal then
         Map.find astar.Goal astar.GScore
    else
        let newAStar = {astar with OpenSet = Set.remove current astar.OpenSet}
        findMinPath (addNeighbors newAStar current (neighbors current))


let minPath risk =
    let start = (0, 0)
    let goal = (Array2D.length1 risk - 1, Array2D.length2 risk - 1)

    let h (row, col) = abs((fst goal) - row) + abs((snd goal) - col)

    let queue = PriorityQueue<int * int, int>()
    queue.Enqueue(start, h start)

    let astar = {
        GScore = Map.ofList [start, 0];
        Queue = queue;
        H = h;
        Risk = risk;
        Goal = goal;
        OpenSet = Set.singleton start;
    }

    findMinPath astar
    

let tile risk =
    let rows = Array2D.length1 risk
    let cols = Array2D.length2 risk
    let tiled = Array2D.create (rows * 5) (cols * 5) 0

    for row = 0 to rows * 5 - 1 do
        let sourceRow = row % rows
        let tileRow = row / rows
        for col = 0 to cols * 5 - 1 do
            let sourceCol = col % cols
            let tileCol = col / cols
            let value = (Array2D.get risk sourceRow sourceCol) + tileRow + tileCol - 1
            Array2D.set tiled row col (value % 9 + 1)

    tiled


[<EntryPoint>]
let main argv =
    let toInt c = c.ToString() |> int
    let risk =
        File.ReadLines(argv.[0])
        |> Seq.map (fun line -> (Seq.map toInt line))
        |> array2D

    printfn "Part 1: %d" (minPath risk)
    printfn "Part 2: %d" (minPath (tile risk))
    0
