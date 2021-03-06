open System.IO


type Bit =
    | One
    | Zero


type Selector =
    | MostCommon
    | LeastCommon


let toBit char =
    match char with
    | '0' -> Some Zero
    | '1' -> Some One
    | _ -> None


let toBits line =
    line
    |> Seq.map toBit
    |> Seq.map Option.get
    |> Seq.toList


let addBit value bit =
    match bit with
    | Zero -> value <<< 1
    | One -> (value <<< 1) + 1


let bitsToInt bits = bits |> List.fold addBit 0


let toSign bit =
    match bit with
    | Zero -> -1
    | One -> 1


let testBits sums bits =
    List.zip sums (List.map toSign bits)
    |> List.map (fun (a, b) -> a + b)


let part1 report =
    let initTest = (List.map toSign (List.head report))
    let bitTest = List.fold testBits initTest (List.tail report)

    let epsilonRate =
        bitTest
        |> List.map (fun test -> if test > 0 then One else Zero)
        |> bitsToInt

    let gammaRate =
        bitTest
        |> List.map (fun test -> if test > 0 then Zero else One)
        |> bitsToInt

    epsilonRate * gammaRate


let filterReportByBit selector index report =
    let bitCounts =
        report
        |> List.map (fun o -> List.item index o)
        |> List.countBy id
        |> Map.ofList

    let filterBit =
        match selector with
        | MostCommon ->
            match bitCounts.[Zero] - bitCounts.[One] with
            | diff when diff > 0 -> Zero
            | diff when diff < 0 -> One
            | _ -> One
        | LeastCommon ->
            match bitCounts.[Zero] - bitCounts.[One] with
            | diff when diff > 0 -> One
            | diff when diff < 0 -> Zero
            | _ -> Zero
        
    List.filter (fun o -> (List.item index o) = filterBit) report


let rec filterReport selector index report =
    match List.length report with
    | 1 -> List.head report
    | _ -> report
           |> filterReportByBit selector index
           |> filterReport selector (index + 1)


let part2 report =
    let oxygenGeneratorRating = 
        report
        |> filterReport MostCommon 0
        |> bitsToInt

    let co2ScrubberRating =
        report
        |> filterReport LeastCommon 0
        |> bitsToInt

    oxygenGeneratorRating * co2ScrubberRating


[<EntryPoint>]
let main argv =
    let report = 
        File.ReadLines(argv.[0])
        |> Seq.map(toBits)
        |> Seq.toList


    printfn "Part 1: %d" (part1 report)
    printfn "Part 2: %d" (part2 report)

    0
