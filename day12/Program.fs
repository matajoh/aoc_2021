open System
open System.IO;


type Cave =
    | Big of string
    | Small of string


let toCave (name : string) =
    match name with
    | name when Char.IsUpper name.[0] -> Big name
    | _ -> Small name


let parseConnection (line : string) =
    let parts = line.Split '-'
    let cave0 = toCave parts.[0]
    let cave1 = toCave parts.[1]
    [cave0, cave1; cave1, cave0]


let canVisit smallCaves twice cave =
    match cave with
    | Small "start" -> false
    | Small _ -> not (twice && Set.contains cave smallCaves)
    | _ -> true


let rec findPaths graph start smallCaves twice path =
    match start with
    | Small "end" -> [List.rev path]
    | _ -> 
        Map.find start graph
        |> List.filter (canVisit smallCaves twice)
        |> List.collect (fun cave ->
            match cave with
            | Small nextCave ->
                let newTwice = twice || Set.contains cave smallCaves
                let newSmallCaves = Set.add cave smallCaves
                findPaths graph cave newSmallCaves newTwice (nextCave :: path)
            | Big nextCave -> findPaths graph cave smallCaves twice (nextCave :: path)
        )


let part1 graph =
    let start = Small "start"
    let smallCaves = Set.ofList [start]
    let twice = true
    let path = ["start"]
    findPaths graph start smallCaves twice path |> List.length


let part2 graph =
    let start = Small "start"
    let smallCaves = Set.ofList [start]
    let twice = false
    let path = ["start"]
    findPaths graph start smallCaves twice path |> List.length


[<EntryPoint>]
let main argv =
    let toLookup (cave, caveList) = cave, Seq.map snd caveList |> Seq.toList
    let graph =
        File.ReadAllLines argv.[0]
        |> Seq.collect parseConnection
        |> Seq.toList
        |> Seq.groupBy fst
        |> Seq.map toLookup
        |> Map.ofSeq

    printfn "Part 1: %d" (part1 graph)
    printfn "Part 2: %d" (part2 graph)
    0
