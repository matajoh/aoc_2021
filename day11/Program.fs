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


let neighbors (row, col) = [
    row - 1, col - 1; row - 1, col; row - 1, col + 1;
    row, col - 1; row, col + 1;
    row + 1, col - 1; row + 1, col; row + 1, col + 1;
]


let enumeratePoints array =
    let rows = Array2D.length1 array
    let cols = Array2D.length2 array
    [0 .. rows - 1]
    |> List.collect (fun row -> 
        [0 .. cols - 1]
        |> List.map (fun col -> row, col))


let incrementOctopus energyLevels (row, col) =
    match tryGet energyLevels (row, col) with
    | Some energy -> Array2D.set energyLevels row col (energy + 1)
    | None -> ()


let increment energyLevels octopi = 
    octopi |> Seq.iter (incrementOctopus energyLevels)


let rec flash energyLevels octopi flashes =
    let notIn flashes octopus = not (Set.contains octopus flashes)
    let unflashed = octopi |> List.filter (notIn flashes)

    let willFlash energyLevels (row, col) = Array2D.get energyLevels row col > 9
    let newFlashes = unflashed |> List.filter (willFlash energyLevels)

    match newFlashes with
    | [] -> Set.toList flashes
    | _ -> 
        newFlashes
        |> List.collect neighbors
        |> (increment energyLevels)
        flash energyLevels unflashed (Set.union flashes (Set.ofList newFlashes))


let reset energyLevels (row, col) = Array2D.set energyLevels row col 0


let rec part1 energyLevels octopi steps =
    match steps with
    | 0 -> 0
    | _ ->
        increment energyLevels octopi
        let flashes = flash energyLevels octopi (Set.ofList [])
        flashes |> List.iter (reset energyLevels)
        (List.length flashes) + (part1 energyLevels octopi (steps - 1))


let rec part2 energyLevels octopi step =
    increment energyLevels octopi
    let flashes = flash energyLevels octopi (Set.ofList [])
    flashes |> List.iter (reset energyLevels)
    match List.length flashes with
    | 100 -> step
    | _ -> part2 energyLevels octopi (step + 1)


[<EntryPoint>]
let main argv =
    let toInt c = c.ToString() |> int
    let energyLevels =
        File.ReadLines(argv.[0])
        |> Seq.map (fun line -> (Seq.map toInt line))
        |> array2D
    let octopi = enumeratePoints energyLevels

    printfn "Part 1: %d" (part1 (Array2D.copy energyLevels) octopi 100)
    printfn "Part 2: %d" (part2 (Array2D.copy energyLevels) octopi 1)
    0
