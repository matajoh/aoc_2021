open System.IO


let parseRule (line : string) =
    let parts = line.Split ' '
    let pair = (parts.[0].[0], parts.[0].[1])
    let insert = parts.[2].[0]
    (pair, insert)


let rec parseRules lines =
    match lines with
    | [] -> []
    | head :: tail -> (parseRule head) :: parseRules tail


let updatePairCount rules (pair, count) =
    match Map.tryFind pair rules with
    | Some c -> [(fst pair, c), count; (c, snd pair), count]
    | None -> [(pair, count)]


let combineCounts counts =
    counts
    |> List.groupBy fst
    |> List.map (fun (k, v) -> k, List.sumBy snd v)


let rec update rules counts steps =
    match steps with
    | 0 -> counts
    | _ ->
        let newCounts =
            counts
            |> List.collect (updatePairCount rules)
            |> combineCounts
        update rules newCounts (steps - 1)


let letterCounts template counts =
    let letterCounts =
        counts
        |> List.collect (fun ((a, b), count) -> [(a, count); (b, count)])

    letterCounts @ [List.head template, 1UL; List.last template, 1UL]
    |> List.map (fun (a, b) -> a, uint64 b)
    |> combineCounts
    |> List.map (fun (a, b) -> (a, b / 2UL))
    |> Map.ofList


let simulate rules template steps =
    let initialCounts =
        List.pairwise template
        |> List.map (fun pair -> (pair, 1UL))

    let counts = update rules initialCounts steps |> (letterCounts template)
    (Seq.max counts.Values) - (Seq.min counts.Values)


[<EntryPoint>]
let main argv =
    let lines =
        File.ReadAllLines argv.[0]
        |> Seq.toList

    let template = (List.head lines) |> Seq.toList
    let rules =
        List.skip 2 lines
        |> parseRules
        |> Map.ofList

    printfn "Part 1: %d" (simulate rules template 10)
    printfn "Part 2: %d" (simulate rules template 40)
    0
