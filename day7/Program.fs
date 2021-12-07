open System.IO


let fuelFor1 position crab = abs (crab - position)


let fuelFor2 position crab =
    let fuel = abs (crab - position)
    ((fuel + 1) * fuel) / 2
    

let totalFuelFor crabs fuelFor position = List.sumBy (fuelFor position) crabs


let findMinFuel crabs fuelFor =
    [ List.head crabs .. List.last crabs ]
    |> List.map (totalFuelFor crabs fuelFor)
    |> List.min


[<EntryPoint>]
let main argv =
    let crabs =
        File.ReadAllText(argv.[0]).Split ','
        |> Seq.map(int)
        |> Seq.sort
        |> Seq.toList

    printfn "Part 1: %d" (findMinFuel crabs fuelFor1)
    printfn "Part 2: %d" (findMinFuel crabs fuelFor2)
    0
