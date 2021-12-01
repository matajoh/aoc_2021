open System
open System.IO


let is_deeper (pair : int array) =
    if (pair.[1] > pair.[0]) then 1
    else 0


let part1 (values : int seq) =
    let depth_pairs = Seq.windowed 2 values
    let sum = Seq.sumBy is_deeper depth_pairs
    sum


let part2 (values : int seq) =
    let depth_triplets = Seq.windowed 3 values
    let depth_sums = depth_triplets |> Seq.map(fun(window) -> Seq.sum window)
    part1 depth_sums


[<EntryPoint>]
let main argv =
    let lines = File.ReadLines(argv.[0])
    let values = lines |> Seq.map(fun(o) -> o |> int)
    let output1 = part1 values
    printfn "Part 1: %d" output1
    let output2 = part2 values
    printfn "Part 2: %d" output2
    0
