open System.IO


let step (timeToSpawn, count) = (timeToSpawn - 1, count)

let grow (fish : uint64 array) =
    [|
        fish.[1];
        fish.[2];
        fish.[3];
        fish.[4];
        fish.[5];
        fish.[6];
        fish.[7] + fish.[0];
        fish.[8];
        fish.[0]
    |]


let rec simulate numDays fish =
    if numDays = 0 then
        Array.sum fish
    else
        simulate (numDays - 1) (grow fish)   


[<EntryPoint>]
let main argv =
    let fish : uint64 array = Array.zeroCreate 9
    File.ReadAllText(argv.[0]).Split ','
    |> Seq.map(int)
    |> Seq.countBy id
    |> Seq.iter (fun (i, count) -> Array.set fish i (uint64(count)))

    printfn "Part 1: %d" (simulate 80 fish)
    printfn "Part 2: %d" (simulate 256 fish)
    0
