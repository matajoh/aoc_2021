open System
open System.IO

let toPoint (line : string) =
    let parts = line.Split ','
    int(parts.[0]), int(parts.[1])


type Fold =
| Left of int
| Up of int


let toFold (line : string) =
    let parts = line.Split ' '
    let bits = parts.[2].Split '='
    let line = bits.[1] |> int
    match bits.[0] with
    | "x" -> Some (Left line)
    | "y" -> Some (Up line)
    | _ -> None


let rec parsePoints lines =
    match lines with
    | [] -> []
    | line :: tail -> (toPoint line) :: (parsePoints tail)


let rec parseFolds lines =
    match lines with
    | [] -> []
    | line :: tail -> (toFold line |> Option.get) :: (parseFolds tail)


let foldPoint fold point =
    match fold with
    | Left line -> 
        match point with
        | (x, y) when x > line -> (line - abs(x - line), y)
        | _ -> point
    | Up line ->
        match point with
        | (x, y) when y > line -> (x, line - abs(y - line))
        | _ -> point


let doFold fold points = points |> Seq.map (foldPoint fold) |> Set.ofSeq


let part1 folds points = doFold (List.head folds) points |> Set.count


let plot points =
    let maxRow = points |> Seq.map snd |> Seq.max
    let maxCol = points |> Seq.map fst |> Seq.max
    let display = Array2D.create (maxRow + 1) (maxCol + 1) '.'
    points |> Seq.iter (fun (col, row) -> Array2D.set display row col '#')
    [|
    for row = 0 to maxRow do
        for col = 0 to maxCol do
            yield (Array2D.get display row col)
        
        yield '\n'
    |] |> String


let rec doFolds folds points =
    match folds with
    | [] -> points
    | fold :: tail -> doFolds tail (doFold fold points)


let rec part2 folds points =
    plot (doFolds folds points)


[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines argv.[0] |> Array.toList
    let split = List.findIndex (fun line -> String.length line = 0) lines
    let points = parsePoints (List.take split lines) |> Set.ofSeq
    let folds = parseFolds (List.skip (split + 1) lines)

    printfn "Part 1: %d" (part1 folds points)
    printfn "Part 2: \n%s" (part2 folds points)
    0
