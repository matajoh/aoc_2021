open System
open System.IO


let is_deeper (pair : int List) =
    match pair with
    | [a; b] when b > a -> 1
    | _ -> 0


let part1 (values : int List) =
    List.sumBy is_deeper (List.windowed 2 values)


let part2 (values : int List) =
    List.windowed 3 values
        |> List.map(fun(window) -> List.sum window)
        |> part1


[<EntryPoint>]
let main argv =
    let values = 
        File.ReadLines(argv.[0])
        |> Seq.map(fun(o) -> o |> int)
        |> Seq.toList
    printfn "Part 1: %d" (part1 values)
    printfn "Part 2: %d" (part2 values)
    0
