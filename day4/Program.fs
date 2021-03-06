open System.IO


type Square =
    | Marked of value: int
    | Unmarked of value: int


let toSquares row (line : string) =
    line.Split ' '
    |> Array.filter (fun s -> s.Length > 0)
    |> Array.map int
    |> Array.toList
    |> List.mapi (fun col index -> row, col, Unmarked index)


let toBoard lines =
    lines
    |> List.skip 1
    |> List.mapi toSquares
    |> List.concat


let rec toBoards lines =
    match lines with
    | [] -> []
    | _ -> toBoard (List.take 6 lines) :: toBoards (List.skip 6 lines)


let markSquare mark (row, col, square) =
    match square with
    | Unmarked value when value = mark -> row, col, Marked value
    | _ -> row, col, square


let markBoards value boards =
    boards |> List.map (List.map (markSquare value))


let markToInt mark =
    match mark with
    | Marked _ -> 1
    | Unmarked _ -> 0


let addRow sum row =
    List.zip sum (List.map markToInt row)
    |> List.map (fun (a, b) -> a + b)


let checkWin squares =
    let initSum = List.map markToInt (List.head squares)
    let sum = List.fold addRow initSum (List.tail squares)
    List.exists (fun sum -> sum = 5) sum


let fst3 (a, _, _) = a
let snd3 (_, b, _) = b
let trd3 (_, _, c) = c


let rows board =
    board
    |> List.groupBy fst3
    |> List.map (fun (_, squares) -> (List.map trd3 squares))


let cols board =
    board
    |> List.groupBy snd3
    |> List.map (fun (_, squares) -> (List.map trd3 squares))


let winningBoard board =
    let rowWin = board |> rows |> checkWin
    let colWin = board |> cols |> checkWin
    rowWin || colWin


let unmarkedValue (_, _, square) =
    match square with
    | Unmarked value -> value
    | Marked _ -> 0


let rec part1 boards (numbers : int list) =
    let newBoards = boards |> markBoards numbers.Head
    let winners = newBoards |> List.filter winningBoard

    match winners with
    | [ ] -> part1 newBoards numbers.Tail
    | _ -> (List.sumBy unmarkedValue winners.Head) * numbers.Head


let rec part2 boards (numbers : int list) =
    let newBoards = boards |> markBoards numbers.Head
    let losers = newBoards |> List.filter (fun o -> not (winningBoard o))

    match losers with
    | [ ] -> (List.sumBy unmarkedValue newBoards.Head) * numbers.Head
    | _ -> part2 losers numbers.Tail


[<EntryPoint>]
let main argv =
    let gameInfo = File.ReadLines(argv.[0]) |> Seq.toList

    let numbers =
        gameInfo.Head.Split ','
        |> Seq.map(int)
        |> Seq.toList

    let boards = gameInfo.Tail |> toBoards

    printfn "Part 1: %d" (part1 boards numbers)
    printfn "Part 2: %d" (part2 boards numbers)
    0
