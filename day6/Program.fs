open System.IO


let reproduce (timeToSpawn, count) =
    if timeToSpawn = 0 then
        [(8, count); (6, count)]
    else
        [(timeToSpawn - 1, count)]

let addFish (timeToSpawn, count0) (_, count1) =
    (timeToSpawn, count0 + count1)

let sumFish populations =
    let pop = List.head populations
    List.fold addFish pop (List.tail populations)


let grow fish =
    fish
    |> List.map reproduce
    |> List.concat
    |> List.groupBy fst
    |> List.map snd
    |> List.map sumFish


let rec simulate numDays fish =
    if numDays = 0 then
        List.sumBy snd fish
    else
        simulate (numDays - 1) (grow fish)   


[<EntryPoint>]
let main argv =
    let fish = 
        File.ReadAllText(argv.[0]).Split ','
        |> Seq.map(int)
        |> Seq.toList
        |> List.countBy id
        |> List.map (fun (timeToSpawn, count) -> (timeToSpawn, uint64(count)))

    printfn "Part 1: %d" (simulate 80 fish)
    printfn "Part 2: %d" (simulate 256 fish)
    0
