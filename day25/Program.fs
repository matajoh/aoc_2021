open System.IO

type SeaCucumber =
    | East
    | South


let parseCucumber char =
    match char with
    | '>' -> Some East
    | 'v' -> Some South
    | _ -> None

let eastOf cols (row, col) =
    match col + 1 with
    | east when east = cols -> (row, 0)
    | east -> (row, east)

let southOf rows (row, col) =
    match row + 1 with
    | south when south = rows -> (0, col)
    | south -> (south, col)


let moveEast cucumbers cols (point, cucumber) =
    match cucumber with
    | East ->
        let east = eastOf cols point
        match Map.tryFind east cucumbers with
        | None -> Some (point, east, cucumber)
        | Some _ -> None
    | _ -> None


let moveSouth cucumbers rows (point, cucumber) =
    match cucumber with
    | South ->
        let south = southOf rows point
        match Map.tryFind south cucumbers with
        | None -> Some (point, south, cucumber)
        | Some _ -> None
    | _ -> None


let move cucumbers (start, finish, cucumber) =
    Map.change start (fun _ -> None) cucumbers
    |> Map.change finish (fun _ -> Some cucumber)


let rec simulate cucumbers rows cols steps =
    let eastMoves =
        Map.toList cucumbers
        |> List.map (moveEast cucumbers cols)
        |> List.filter Option.isSome
        |> List.map Option.get

    let eastMoved = List.fold move cucumbers eastMoves

    let southMoves =
        Map.toList eastMoved
        |> List.map (moveSouth eastMoved rows)
        |> List.filter Option.isSome
        |> List.map Option.get

    let newCucumbers = List.fold move eastMoved southMoves

    match List.length eastMoves + List.length southMoves with
    | 0 -> steps
    | _ -> simulate newCucumbers rows cols (steps + 1)


[<EntryPoint>]
let main argv =
    let seaFloor =
        File.ReadAllLines argv.[0]
        |> Seq.mapi (fun row line ->
            line
            |> Seq.mapi (fun col char ->
                (row, col), char)
            |> Seq.toList)
        |> Seq.toList

    let rows = List.length seaFloor
    let cols = List.length (List.head seaFloor)

    let cucumbers =
        seaFloor
        |> List.concat
        |> List.map (fun (k, v) -> k, parseCucumber v)
        |> List.filter (snd >> Option.isSome)
        |> List.map (fun (k, v) -> k, Option.get v)
        |> Map.ofList

    printfn "Part 1: %d" (simulate cucumbers rows cols 1)
    0
