open System.IO

type SeaCucumber =
    | East
    | South
    | Empty


let parseCucumber char =
    match char with
    | '>' -> East
    | 'v' -> South
    | _ -> Empty


let eastOf cols (row, col) =
    match col + 1 with
    | east when east = cols -> (row, 0)
    | east -> (row, east)


let southOf rows (row, col) =
    match row + 1 with
    | south when south = rows -> (0, col)
    | south -> (south, col)


let moveEast cucumbers (point, cucumber) =
    let cols = Array2D.length2 cucumbers
    match cucumber with
    | East ->
        let row, col = eastOf cols point
        match Array2D.get cucumbers row col with
        | Empty -> Some (point, (row, col), cucumber)
        | _ -> None
    | _ -> None


let moveSouth cucumbers (point, cucumber) =
    let rows = Array2D.length1 cucumbers
    match cucumber with
    | South ->
        let row, col = southOf rows point
        match Array2D.get cucumbers row col with
        | Empty -> Some (point, (row, col), cucumber)
        | _ -> None
    | _ -> None


let move cucumbers ((row0, col0), (row1, col1), cucumber) =
    Array2D.set cucumbers row0 col0 Empty
    Array2D.set cucumbers row1 col1 cucumber


let enumerate cucumbers =
    let rows = Array2D.length1 cucumbers
    let cols = Array2D.length2 cucumbers
    seq {
        for row = 0 to rows - 1 do
            for col = 0 to cols - 1 do
                match Array2D.get cucumbers row col with
                | Empty -> ()
                | cucumber -> yield (row, col), cucumber
    }


let rec simulate cucumbers steps =
    let eastMoves =
        enumerate cucumbers
        |> Seq.map (moveEast cucumbers)
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Seq.toList

    List.iter (move cucumbers) eastMoves

    let southMoves =
        enumerate cucumbers
        |> Seq.map (moveSouth cucumbers)
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Seq.toList

    List.iter (move cucumbers) southMoves

    match List.length eastMoves + List.length southMoves with
    | 0 -> steps
    | _ -> simulate cucumbers (steps + 1)


[<EntryPoint>]
let main argv =
    let cucumbers =
        File.ReadAllLines argv.[0]
        |> Seq.map (fun line ->
            Seq.map parseCucumber line
            |> Seq.toList)

        |> Seq.toList
        |> array2D

    printfn "Part 1: %d" (simulate cucumbers 1)
    0
