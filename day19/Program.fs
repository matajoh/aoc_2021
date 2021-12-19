open System.IO

let rec parseBeacons sensors beacons lines =
    match lines with
    | [] -> List.rev ((List.rev beacons) :: sensors)
    | "" :: tail -> parseBeacons ((List.rev beacons) :: sensors) [] tail
    | head :: tail ->
        if head.StartsWith "---" then
            parseBeacons sensors beacons tail
        else
            let beacon = head.Split ','
                        |> Seq.map int
                        |> Array.ofSeq
            parseBeacons sensors ((beacon.[0], beacon.[1], beacon.[2]) :: beacons) tail


let rotate (m00, m01, m02, m10, m11, m12, m20, m21, m22) (x, y, z) =
    (
        m00 * x + m01 * y + m02 * z,
        m10 * x + m11 * y + m12 * z,
        m20 * x + m21 * y + m22 * z
    )


let apply op (x0, y0, z0) (x1, y1, z1) =
    op x0 x1, op y0 y1, op z0 z1

let subtract a b = apply (-) a b
let add a b = apply (+) a b


let Rotations =
    [
        -1,0,0,0,-1,0,0,0,1;
        0,1,0,-1,0,0,0,0,1;
        0,0,1,1,0,0,0,1,0;
        0,1,0,1,0,0,0,0,-1;
        0,-1,0,1,0,0,0,0,1;
        1,0,0,0,0,1,0,-1,0;
        0,-1,0,-1,0,0,0,0,-1;
        0,1,0,0,0,-1,-1,0,0;
        0,-1,0,0,0,-1,1,0,0;
        0,0,-1,0,-1,0,-1,0,0;
        0,0,1,-1,0,0,0,-1,0;
        0,-1,0,0,0,1,-1,0,0;
        0,0,-1,-1,0,0,0,1,0;
        0,0,1,0,-1,0,1,0,0;
        0,0,1,0,1,0,-1,0,0;
        -1,0,0,0,0,1,0,1,0;
        -1,0,0,0,0,-1,0,-1,0;
        0,0,-1,0,1,0,1,0,0;
        1,0,0,0,1,0,0,0,1;
        1,0,0,0,-1,0,0,0,-1;
        -1,0,0,0,1,0,0,0,-1;
        1,0,0,0,0,-1,0,1,0;
        0,1,0,0,0,1,1,0,0;
        0,0,-1,1,0,0,0,-1,0;
    ]


let inc value =
    match value with
    | None -> Some 1
    | Some count -> Some (count + 1)


let rec testTransform beacons counts =
    match beacons with
    | [] -> None
    | (a, b) :: tail ->
        let diff = subtract a b
        let newCounts = Map.change diff inc counts
        if Map.find diff newCounts = 12 then
            Some diff
        else
            testTransform tail newCounts


let product a b =
    [
        for i in a do
            for j in b do
                yield i, j
    ]


let selfProduct a =
    let length = List.length a
    [
        for i = 0 to length - 1 do
            for j = (i + 1) to length - 1 do
                yield a[i], a[j]
    ]


let rec findTransform beacons0 beacons1 rotations =
    match rotations with
    | [] -> None
    | R :: tail ->
        let rotated = beacons1 |> List.map (rotate R)
        match testTransform (product beacons0 rotated) Map.empty with
        | Some t -> Some (R, t)
        | None -> findTransform beacons0 beacons1 tail


let rec findMatch positions beacons =
    match beacons with
    | [] -> positions, [], None
    | head :: tail ->
        match findTransform positions head Rotations with
        | Some (R, t) ->
            let newPositions =
                List.map (rotate R) head
                |> List.map (add t)
                |> Set.ofList
                |> Set.union (Set.ofList positions)
                |> Set.toList
            newPositions, tail, Some t
        | None ->
            let newPositions, remain, maybeT = findMatch positions tail
            newPositions, head :: remain, maybeT


let rec findPositions positions beacons scanners =
    match beacons with
    | [] -> positions, scanners
    | _ ->
        let newPositions, remain, scanner = findMatch positions beacons
        findPositions newPositions remain (Option.get scanner :: scanners)


let manhattan (a, b) =
    let dx, dy, dz = subtract a b
    (abs dx) + (abs dy) + (abs dz)


[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines argv.[0] |> Seq.toList
    let beacons = parseBeacons [] [] lines

    let positions, scanners = findPositions (List.head beacons) (List.tail beacons) []
    printfn "Part 1: %d" (List.length positions)
    printfn "Part 2: %d" (selfProduct scanners |> List.map manhattan |> List.max)
    0
