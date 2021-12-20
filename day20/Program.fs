open System.Collections.Generic
open System.IO


let rec readBits line =
    match line with
    | '#' :: tail -> true :: (readBits tail)
    | '.' :: tail -> false :: (readBits tail)
    | _ -> []


let getPixel image pixel =
    match Map.tryFind pixel image with
    | Some bit -> bit
    | None -> false


let window (row, col) =
    [
        row - 1, col - 1; row - 1, col; row - 1, col + 1;
        row, col - 1; row, col; row, col + 1;
        row + 1, col - 1; row + 1, col; row + 1, col + 1;
    ]


let rec toInt value bits =
    match bits with
    | [] -> value
    | true :: tail -> toInt (2 * value + 1) tail
    | false :: tail -> toInt (2 * value) tail


let readImage lines =
    lines
    |> List.mapi (fun row line ->
        readBits line
        |> List.mapi (fun col bit -> ((row, col), bit)))
    |> List.concat
    |> List.filter snd
    |> Map.ofList


let enumeratePixels image border =
    let keys = Map.keys image
    let minRow = keys |> Seq.map fst |> Seq.min
    let maxRow = keys |> Seq.map fst |> Seq.max
    let minCol = keys |> Seq.map snd |> Seq.min
    let maxCol = keys |> Seq.map snd |> Seq.max
    seq {
        for row = minRow - border to maxRow + border do
            for col = minCol - border to maxCol + border do
                yield (row, col)
    }


let Pixels = Dictionary<int * (int * int), bool>()
let rec enhancePixel algorithm image step pixel =
    let key = step, pixel
    let exists, value = Pixels.TryGetValue(key)
    match exists with
    | true -> value
    | false ->
        let newValue =
            match step with
            | 0 -> getPixel image pixel
            | _ ->
                window pixel
                |> List.map (enhancePixel algorithm image (step - 1))
                |> toInt 0
                |> Array.get algorithm
        Pixels.Add(key, newValue)
        newValue


let rec enhance algorithm image steps =
    match steps with
    | 0 -> image
    | _ ->
        Pixels.Clear()
        let newImage =
            enumeratePixels image 2
            |> Seq.map (fun pixel -> pixel, enhancePixel algorithm image 2 pixel)
            |> Seq.filter snd
            |> Map.ofSeq
        enhance algorithm newImage (steps - 2)


[<EntryPoint>]
let main argv =
    let lines =
        File.ReadAllLines argv.[0]
        |> Seq.map (Seq.toList)
        |> Seq.toList

    let algorithm = readBits (List.head lines) |> Array.ofList
    let image = readImage (List.skip 2 lines)
    printfn "Part 1: %d" (enhance algorithm image 2 |> Map.count)
    printfn "Part 2: %d" (enhance algorithm image 50 |> Map.count)
    0
