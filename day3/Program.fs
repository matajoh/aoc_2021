open System
open System.IO


type Bit =
    | One
    | Zero


type Selector =
    | MostCommon
    | LeastCommon


let toBit char =
    match char with
    | '0' -> Some(Zero)
    | '1' -> Some(One)
    | _ -> None


let toBits (line : String) =
    line
    |> Seq.map toBit
    |> Seq.map Option.get
    |> Seq.indexed
    |> Seq.toList


let maxBit (bitCounts : ((int * Bit) * int) list) =
    bitCounts
    |> List.maxBy (fun ((_, _), count) -> count)
    |> fun ((_, bit), _) -> bit


let flipBit (index,  bit) =
    match bit with
    | Zero -> index, One
    | One -> index, Zero


let addBit value bit =
    match bit with
    | Zero -> value <<< 1
    | One -> (value <<< 1) + 1


let bitsToInt bits =
    bits
    |> List.sortBy fst
    |> List.map snd
    |> List.fold addBit 0


let part1 report =
    let bitCounts =
        report
        |> List.concat
        |> List.countBy (fun o -> o)

    let epsilonBits =
        bitCounts
        |> List.groupBy (fun ((index, _),  _) -> index)
        |> List.map (fun (index, bitList) -> index, maxBit bitList)

    let gammaBits =
        epsilonBits
        |> List.map flipBit

    let epsilonRate = bitsToInt epsilonBits
    let gammaRate = bitsToInt gammaBits

    epsilonRate * gammaRate


let filterReportByBit selector index (report : (int * Bit) list list) =
    let bitCounts =
        report
        |> List.map (List.filter (fun o -> fst o = index))
        |> List.map (fun o -> snd o.Head)
        |> List.countBy (fun o -> o)
        |> Map.ofList

    let filterBit =
        match selector with
        | MostCommon -> if bitCounts.[Zero] > bitCounts.[One] then
                            Zero 
                        else if bitCounts.[One] > bitCounts.[Zero] then
                            One
                        else
                            One
        | LeastCommon -> if bitCounts.[Zero] < bitCounts.[One] then
                            Zero
                         else if bitCounts.[One] < bitCounts[Zero] then
                            One
                         else
                            Zero
        
    report
    |> List.filter (fun o -> snd o.[index] = filterBit)


let rec filterReport selector index (report : (int * Bit) list list) =
    match report.Length with
    | 1 -> report.Head
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
    let diagnostics = 
        File.ReadLines(argv.[0])
        |> Seq.map(toBits)
        |> Seq.toList


    printfn "Part 1: %d" (part1 diagnostics)
    printfn "Part 2: %d" (part2 diagnostics)

    0
