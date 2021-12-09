open System.IO


let tryGet array (row, col) =
    let rows = Array2D.length1 array
    let cols = Array2D.length2 array
    match row, col with
    | (row, _) when row < 0 -> None
    | (row, _) when row >= rows -> None
    | (_, col) when col < 0 -> None
    | (_, col) when col >= cols -> None
    | (row, col) -> Some (Array2D.get array row col)


let neighbors row col = [row - 1, col; row + 1, col; row, col + 1; row, col - 1]


let isLower depths queryDepth point =
    match tryGet depths point with
    | Some depth -> depth <= queryDepth
    | None -> false


let isLowestPoint depths (row, col) =
    let depth = Array2D.get depths row col
    neighbors row col
    |> List.map (isLower depths depth)
    |> List.exists id
    |> (not)


let enumeratePoints array =
    let rows = Array2D.length1 array
    let cols = Array2D.length2 array
    [0 .. rows - 1]
    |> List.collect (fun row -> 
                        [0 .. cols - 1]
                        |> List.map (fun col -> row, col))


let lowestPoints depths =
    enumeratePoints depths
    |> Seq.filter (isLowestPoint depths)


let part1 depths =
    lowestPoints depths
    |> Seq.map (fun (row, col) -> (Array2D.get depths row col) + 1)
    |> Seq.sum


let isInBasin depths queryDepth point =
    match tryGet depths point with
    | Some depth when depth < 9 -> depth > queryDepth
    | _ -> false


let validNeighbors basin depths (row, col) =
    let depth = Array2D.get depths row col
    neighbors row col
    |> List.filter (fun p -> not (Set.contains p basin))
    |> List.filter (isInBasin depths depth)


let rec fillBasin frontier basin depths =
    match frontier with
    | [] -> basin
    | head :: tail ->
        let newFrontier = (validNeighbors basin depths head) @ tail
        let newBasin = Set.add head basin
        fillBasin newFrontier newBasin depths


let measureBasinSize depths bottom =
    fillBasin [bottom] Set.empty depths |> Set.count


let part2 depths =
    lowestPoints depths
    |> Seq.map (measureBasinSize depths)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.fold (fun prod a -> prod * a) 1


[<EntryPoint>]
let main argv =
    let toInt c = c.ToString() |> int
    let depths =
        File.ReadLines(argv.[0])
        |> Seq.map (fun line -> (Seq.map toInt line))
        |> array2D

    printfn "Part 1: %d" (part1 depths)
    printfn "Part 2: %d" (part2 depths)
    0
