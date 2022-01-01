open System.IO


type Vent =
    | Horizontal of points : (int * int) list
    | Vertical of points : (int * int) list
    | Diagonal of points : (int * int) list


let getPoints includeDiag vent =
    match vent with
    | Horizontal points -> points
    | Vertical points -> points
    | Diagonal points -> if includeDiag then points else []


let toPoint (vals : int[]) = vals.[0], vals.[1]


let enumerate x0 x1 =
    if x0 < x1 then
        [x0 .. x1]
    else
        [x0 .. -1 .. x1]


let horizontal x0 x1 y =
    enumerate x0 x1
    |> List.map (fun x -> x, y)
    |> Horizontal


let vertical x y0 y1 =
    enumerate y0 y1
    |> List.map (fun y -> x, y)
    |> Vertical


let diagonal x0 x1 y0 y1 =
    List.zip (enumerate x0 x1) (enumerate y0 y1)
    |> Diagonal


let toVent (line : string) =
    let parts = line.Split ' '
    let (x0, y0) = parts.[0].Split ',' |> Array.map int |> toPoint
    let (x1, y1) = parts.[2].Split ',' |> Array.map int |> toPoint
    let diffs = x1 - x0, y1 - y0
    match diffs with
    | (_, 0) -> horizontal x0 x1 y0
    | (0, _) -> vertical x0 y0 y1
    | _ -> diagonal x0 x1 y0 y1


let countIntersections includeDiag vents =
    vents
    |> Seq.collect (getPoints includeDiag)
    |> Seq.countBy id
    |> Seq.sumBy (fun (_, count) -> if count > 1 then 1 else 0)


[<EntryPoint>]
let main argv =
    let vents = 
        File.ReadLines(argv.[0])
        |> Seq.map toVent
        |> Seq.toList

    printfn "Part 1: %d" (countIntersections false vents)
    printfn "Part 2: %d" (countIntersections true vents)
    0
