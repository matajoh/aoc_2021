open System.IO

let parseTargetArea (line : string) =
    let parts = line.Split ' '
    let x = parts.[2].Split ('=', ',')
    let y = parts.[3].Split '='
    let x_minmax = x.[1].Split ".."
    let y_minmax = y.[1].Split ".."
    let x_min = x_minmax.[0] |> int
    let x_max = x_minmax.[1] |> int
    let y_min = y_minmax.[0] |> int
    let y_max = y_minmax.[1] |> int
    (x_min, x_max), (y_min, y_max)


let maxVal v = (v * (v + 1)) / 2
let y v s = s * v - (s * (s + 1)) / 2 + s
let x v s =
    match s with
    | s when s >= v -> maxVal v
    | _ -> y v s


let calculateXVelocity x = (-1. + sqrt (1. + 8. * float(x))) / 2.


let rec isValidYVelocity (minY, maxY) y v =
    match y with
    | y when y < minY -> false
    | y when y > maxY -> isValidYVelocity (minY, maxY) (y + v) (v - 1)
    | _ -> true


let rec findMaxVelocity minV maxV valid =
    match minV, maxV with
    | minV, maxV when maxV <= minV -> minV
    | minV, maxV when minV + 1 = maxV ->
        if valid maxV then maxV else minV
    | _ -> 
        let midV = (minV + maxV) / 2
        if valid midV then
            findMaxVelocity midV maxV valid
        else
            findMaxVelocity minV midV valid


let rec findMinVelocity minV maxV valid =
    match minV, maxV with
    | minV, maxV when maxV <= minV -> minV
    | minV, maxV when minV + 1 = maxV ->
        if valid minV then minV else maxV
    | _ -> 
        let midV = (minV + maxV) / 2
        if valid midV then
            findMinVelocity minV midV valid
        else
            findMinVelocity midV maxV valid


let rec validYSteps (minY, maxY) step y v =
    match y with
    | y when y < minY -> []
    | y when y > maxY -> validYSteps (minY, maxY) (step + 1) (y + v) (v - 1)
    | _ -> step :: validYSteps (minY, maxY) (step + 1) (y + v) (v - 1)


let inRange (floor, ceil) value = not (value < floor || value > ceil)


let validInitialVelocity xRange yRange xv yv =
    let isValid =
        validYSteps yRange 0 0 yv
        |> List.map (x xv)
        |> List.filter (inRange xRange)
        |> List.isEmpty |> not

    if isValid then Some (xv, yv) else None


let validInitialVelocities xRange yRange xvs yvs =
    xvs
    |> List.collect (fun xv -> yvs |> List.map (validInitialVelocity xRange yRange xv))
    |> List.filter Option.isSome
    |> List.map Option.get


[<EntryPoint>]
let main argv =
    let xRange, yRange =
        File.ReadAllText argv.[0]
        |> parseTargetArea

    let maxYV = findMaxVelocity 0 100 (isValidYVelocity yRange 0)
    printfn "Part 1: %d" (maxVal maxYV)

    let minYV = findMinVelocity -100 0 (isValidYVelocity yRange 0)
    let minXV = calculateXVelocity (fst xRange) |> int |> (+) 1
    let maxXV = snd xRange
    let velocities = validInitialVelocities xRange yRange [minXV .. maxXV] [minYV .. maxYV]
    printfn "Part 2: %d" (List.length velocities)
    0
