open System.IO


let is_deeper (pair : int List) =
    match pair with
    | [a; b] when b > a -> 1
    | _ -> 0


let part1 (values : int List) =
    List.sumBy is_deeper (List.windowed 2 values)


let part2 (values : int List) =
    List.windowed 3 values
        |> List.map(List.sum)
        |> part1


[<EntryPoint>]
let main argv =
    let values = 
        File.ReadLines(argv.[0])
        |> Seq.map(int)
        |> Seq.toList
    printfn "Part 1: %i" (part1 values)
    printfn "Part 2: %i" (part2 values)
    0
