open System.IO

type Number =
    | Pair of Number * Number
    | Regular of int


let rec toString number =
    match number with
    | Pair (left, right) -> sprintf "[%s,%s]" (toString left) (toString right)
    | Regular value -> sprintf "%d" value


let rec parseNumber index (line: string) =
    match line.[index] with
    | '[' ->
        let left, comma = parseNumber (index + 1) line
        let right, bracket = parseNumber (comma + 1) line
        Pair (left, right), bracket + 1
    | _ ->
        Regular (line.[index..index] |> int), index + 1


let rec addToLeft number extra =
    match number with
    | Regular value -> Regular (value + extra)
    | Pair (left, right) -> Pair (addToLeft left extra, right)


let rec addToRight number extra =
    match number with
    | Regular value -> Regular (value + extra)
    | Pair (left, right) -> Pair (left, addToRight right extra)


let rec explode number nest =
    match number, nest with
    | Pair (Regular value, Pair (Regular left, Regular right)), 3 ->
        Some (Pair (Regular (value + left), Regular 0), 0, right)
    | Pair (Pair (Regular left ,Regular  right), Regular value), 3 ->
        Some (Pair (Regular 0, Regular (value + right)), left, 0)
    | Pair (Pair (Regular left, Regular right), other), 3 ->
        Some (Pair (Regular 0, addToLeft other right), left, 0)
    | Pair (Regular value, right), _ ->
        match explode right (nest + 1) with
        | Some (exploded, left, right) ->
            Some (Pair(Regular (left + value), exploded), 0, right)
        | None -> None
    | Pair (left, Regular value), _ ->
        match explode left (nest + 1) with
        | Some (exploded, left, right) ->
            Some (Pair(exploded, Regular (right + value)), left, 0)
        | None -> None
    | Pair (left, right), _ ->
        match explode left (nest + 1) with
        | Some (exploded, addL, addR) ->
            Some (Pair(exploded, addToLeft right addR), addL, 0)
        | None ->
            match explode right (nest + 1) with
            | Some (exploded, addL, addR) ->
                Some (Pair(addToRight left addL, exploded), 0, addR)
            | None -> None
    | _ -> None


let rec split number =
    match number with
    | Regular value when value > 9 ->
        let half = value |> float |> (*) 0.5
        let left = half |> floor |> int
        let right = half |> ceil |> int
        Some (Pair (Regular left, Regular right))
    | Regular _ -> None
    | Pair (left, right) ->
        match split left with
        | Some leftSplit -> Some (Pair (leftSplit, right))
        | None ->
            match split right with
            | Some rightSplit -> Some (Pair (left, rightSplit))
            | None -> None


let rec reduce number =
    match explode number 0 with
    | Some (explodedNumber, _, _) -> reduce explodedNumber
    | None ->
        match split number with
        | Some splitNumber -> reduce splitNumber
        | None -> number


let add left right = reduce (Pair (left, right))


let rec magnitude number =
    match number with
    | Regular value -> value
    | Pair (left, right) -> 3 * (magnitude left) + 2 * (magnitude right)


let part1 numbers =
    List.fold add (List.head numbers) (List.tail numbers)
    |> magnitude


let part2 numbers =
    [
        for left in numbers do
            for right in numbers do
                yield magnitude (add left right)
    ] |> List.max


[<EntryPoint>]
let main argv =
    let numbers =
        File.ReadAllLines argv.[0]
        |> Seq.map (parseNumber 0)
        |> Seq.map fst
        |> Seq.toList

    printfn "Part 1: %d" (part1 numbers)
    printfn "Part 2: %d" (part2 numbers)
    0
