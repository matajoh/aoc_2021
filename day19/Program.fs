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
            parseBeacons sensors (beacon :: beacons) tail


let rotate (r : int []) (p : int[]) =
    [| 
        r.[0] * p.[0] + r.[1] * p.[1] + r.[2] * p.[2];
        r.[3] * p.[0] + r.[4] * p.[1] + r.[5] * p.[2];
        r.[6] * p.[0] + r.[7] * p.[1] + r.[8] * p.[2];
    |]


let apply op (a : int[]) (b : int[]) =
    [| op a.[0] b.[0]; op a.[1] b.[1]; op a.[2] b.[2] |]

let subtract a b = apply (-) a b
let add a b = apply (+) a b


let Rotations =
    [
        [|-1;0;0;0;-1;0;0;0;1|];
        [|0;1;0;-1;0;0;0;0;1|];
        [|0;0;1;1;0;0;0;1;0|]; 
        [|0;1;0;1;0;0;0;0;-1|];
        [|0;-1;0;1;0;0;0;0;1|];
        [|1;0;0;0;0;1;0;-1;0|];
        [|0;-1;0;-1;0;0;0;0;-1|];
        [|0;1;0;0;0;-1;-1;0;0|];
        [|0;-1;0;0;0;-1;1;0;0|];
        [|0;0;-1;0;-1;0;-1;0;0|];
        [|0;0;1;-1;0;0;0;-1;0|];
        [|0;-1;0;0;0;1;-1;0;0|];
        [|0;0;-1;-1;0;0;0;1;0|];
        [|0;0;1;0;-1;0;1;0;0|];
        [|0;0;1;0;1;0;-1;0;0|];
        [|-1;0;0;0;0;1;0;1;0|];
        [|-1;0;0;0;0;-1;0;-1;0|];
        [|0;0;-1;0;1;0;1;0;0|];
        [|1;0;0;0;1;0;0;0;1|];
        [|1;0;0;0;-1;0;0;0;-1|];
        [|-1;0;0;0;1;0;0;0;-1|];
        [|1;0;0;0;0;-1;0;1;0|];
        [|0;1;0;0;0;1;1;0;0|];
        [|0;0;-1;1;0;0;0;-1;0|];
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
        let key = diff.[0], diff.[1], diff.[2]
        let newCounts = Map.change key inc counts
        if Map.find key newCounts = 12 then
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


let toSet (beacons : int [] list) =
    beacons
    |> List.map (fun x -> x.[0], x.[1], x.[2])
    |> Set.ofList


let toList beacons =
    beacons
    |> Set.toList
    |> List.map (fun (x, y, z) -> [|x; y; z;|])
 

let rec findMatch positions beacons =
    match beacons with
    | [] -> positions, [], None
    | head :: tail ->
        match findTransform positions head Rotations with
        | Some (R, t) ->
            let newPositions =
                List.map (rotate R) head
                |> List.map (add t)
                |> toSet
                |> Set.union (toSet positions)
                |> toList
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


let manhattan (a, b) = subtract a b |> Array.sumBy abs


[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines argv.[0] |> Seq.toList
    let beacons = parseBeacons [] [] lines

    let positions, scanners = findPositions (List.head beacons) (List.tail beacons) []
    printfn "Part 1: %d" (List.length positions)
    printfn "Part 2: %d" (selfProduct scanners |> List.map manhattan |> List.max)
    0
