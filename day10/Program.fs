open System.IO


type Delim =
    | Open of char
    | Close of char


let toDelim char =
    match char with
    | '(' -> Some (Open ')')
    | ')' -> Some (Close ')')
    | '[' -> Some (Open ']')
    | ']' -> Some (Close ']')
    | '{' -> Some (Open '}')
    | '}' -> Some (Close '}')
    | '<' -> Some (Open '>')
    | '>' -> Some (Close '>')
    | _ -> None


let parseDelims line =
    line
    |> Seq.map (toDelim >> Option.get)
    |> Seq.toList


let rec parseChunks opens line =
    match line with
    | [] -> None, opens
    | Close closeDelim :: tail -> 
        match List.tryHead opens with
        | Some openDelim when openDelim <> closeDelim -> Some closeDelim, opens
        | None -> Some closeDelim, opens
        | _ -> parseChunks (List.tail opens) tail
    | Open openDelim :: tail -> parseChunks (openDelim :: opens) tail


let charToScore maybeChar =
    match maybeChar with
    | Some ')' -> 3
    | Some ']' -> 57
    | Some '}' -> 1197
    | Some '>' -> 25137
    | _ -> 0


let part1 chunks =
    chunks
    |> List.map fst
    |> List.filter Option.isSome
    |> List.map charToScore
    |> List.sum


let rec scoreLine score line =
    match line with
    | [] -> score
    | ')' :: tail -> scoreLine (score * 5UL + 1UL) tail
    | ']' :: tail -> scoreLine (score * 5UL + 2UL) tail
    | '}' :: tail -> scoreLine (score * 5UL + 3UL) tail
    | '>' :: tail -> scoreLine (score * 5UL + 4UL) tail
    | _ -> 0UL


let part2 chunks =
    let scores =
        chunks
        |> List.filter (fun (illegal, _) -> Option.isNone illegal)
        |> List.map snd
        |> List.map (scoreLine 0UL)
        |> List.sort

    scores.[scores.Length / 2]


[<EntryPoint>]
let main argv =
    let chunks =
        File.ReadAllLines argv.[0]
        |> Seq.map parseDelims
        |> Seq.map (parseChunks [])
        |> Seq.toList

    printfn "Part 1: %d" (part1 chunks)
    printfn "Part 2: %d" (part2 chunks)
    0
