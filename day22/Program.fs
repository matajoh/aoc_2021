open System.IO


type Cuboid = {
    XRange: int * int;
    YRange: int * int;
    ZRange: int * int;
}


let maxmin (min0, _) (min1, _) = max min0 min1
let minmax (_, max0) (_, max1) = min max0 max1


let intersect a b =
    let xmin = maxmin a.XRange b.XRange
    let ymin = maxmin a.YRange b.YRange
    let zmin = maxmin a.ZRange b.ZRange
    let xmax = minmax a.XRange b.XRange
    let ymax = minmax a.YRange b.YRange
    let zmax = minmax a.ZRange b.ZRange
    if xmin <= xmax then
        if ymin <= ymax then
            if zmin <= zmax then
                Some {XRange=(xmin, xmax);
                      YRange=(ymin, ymax);
                      ZRange=(zmin, zmax)}
            else
                None
        else
            None
    else
        None


let length (a, b) = b - a + 1 |> int64
let volume cuboid =
    (length cuboid.XRange) * (length cuboid.YRange) * (length cuboid.ZRange)


type Step =
    | On of Cuboid
    | Off of Cuboid


let parseRange (text : string) =
    let parts = text.Split "="
    let minMax =
        parts.[1].Split ".."
        |> Array.map int
    minMax.[0], minMax.[1]


let parseCuboid (text : string) =
    let ranges = text.Split ','
    {
        XRange = parseRange ranges.[0];
        YRange = parseRange ranges.[1];
        ZRange = parseRange ranges.[2]
    }


let rec parseStep (line : string) =
    let parts = line.Split ' '
    let cuboid = parseCuboid parts.[1]
    match parts.[0] with
    | "on" -> Some (On cuboid)
    | "off" -> Some (Off cuboid)
    | _ -> None


let intersectList cuboids cuboid =
    cuboids
    |> List.map (intersect cuboid)
    |> List.filter Option.isSome
    |> List.map Option.get


let rec sumList cuboids =
    match cuboids with
    | [] -> 0L
    | head :: tail ->
        let toAdd = (volume head) - (intersectList tail head |> sumList)
        toAdd + (sumList tail)


let outside (min0, max0) (min1, max1) =
    min1 < min0 || max1 > max0


let contains b a =
    let xOut = outside a.XRange b.XRange
    let yOut = outside a.YRange b.YRange
    let zOut = outside a.ZRange b.ZRange
    not (xOut || yOut || zOut)


let rec reduce cuboids =
    match cuboids with
    | [] -> []
    | head :: tail ->
        if List.exists (contains head) tail then
            reduce tail
        else
            head :: reduce tail


let rec switchOn onOff isOn cuboids =
    match onOff, isOn with
    | [], true -> if List.length cuboids > 0 then [cuboids] else []
    | [], false -> []
    | on :: tail, true ->
        (cuboids @ on) :: switchOn tail false cuboids
    | off :: tail, false ->
        off :: (
            cuboids
            |> List.collect (intersectList off)
            |> switchOn tail true)


let rec switchOff onOff isOn cuboids =
    match onOff, isOn with
    | [], true -> []
    | [], false -> if List.length cuboids > 0 then [cuboids] else []
    | on :: tail, true ->
        on :: (
            cuboids
            |> List.collect (intersectList on)
            |> switchOff tail false
        )
    | off :: tail, false ->
        (cuboids @ off) :: switchOff tail true cuboids


let onOffSum i cuboids =
    let sum = reduce cuboids |> sumList
    if i % 2 = 0 then
        sum
    else
        -sum


let rec execute onOff steps =
    match steps with
    | [] ->
        onOff
        |> List.mapi onOffSum
        |> List.sum
    | On cuboid :: tail -> execute (switchOn onOff true [cuboid]) tail
    | Off cuboid :: tail -> execute (switchOff onOff true [cuboid]) tail


let intersectStep region step =
    match step with
    | On cuboid ->
        match intersect region cuboid with
        | Some valid -> Some (On valid)
        | None -> None
    | Off cuboid ->
        match intersect region cuboid with
        | Some valid -> Some (Off valid)
        | None -> None


let part1 steps =
    let region = {XRange = (-50, 50); YRange = (-50, 50); ZRange = (-50, 50)}
    let initSteps =
        steps
        |> List.map (intersectStep region)
        |> List.filter Option.isSome
        |> List.map Option.get

    execute [] initSteps


let part2 steps =
    execute [] steps


[<EntryPoint>]
let main argv =
    let steps =
        File.ReadAllLines argv.[0]
        |> Seq.map (parseStep >> Option.get)
        |> Seq.toList

    printfn "Part 1: %d" (part1 steps)
    printfn "Part 2: %d" (part2 steps)
    0
