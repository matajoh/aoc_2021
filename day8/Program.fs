open System
open System.IO


let parsePatterns (patterns : string) =
    let notEmpty s = String.length s > 0
    let toSet s = s |> Seq.toList |> Set.ofList
    patterns.Split ' '
    |> Array.filter notEmpty
    |> Array.map toSet
    |> Array.toList


let parseNote (line : string) =
    let patterns = line.Split '|' |> Array.map parsePatterns
    patterns.[0], patterns.[1]


let isEasy pattern =
    match Set.count pattern with
    | 2 -> 1
    | 3 -> 1
    | 4 -> 1
    | 7 -> 1
    | _ -> 0


let part1 notes =
    let countEasyDigits patterns = List.sumBy isEasy patterns
    notes
    |> List.map snd
    |> List.sumBy countEasyDigits


let unwrapSingleton list =
    match list with
    | [ singleton ] -> Some singleton
    | _ -> None


let findUniqueIntersectionOfCount count query patterns =
    let isMatch p = Set.count (Set.intersect query p) = count
    patterns |> List.filter isMatch |> unwrapSingleton |> Option.get


let deduceWirePattern uniquePatterns =
    let patternsByCount =
        uniquePatterns
        |> List.groupBy Set.count
        |> Map.ofList

    let cf = Map.find 2 patternsByCount |> List.head                 // one
    let acf = Map.find 3 patternsByCount |> List.head                // seven
    let abdefg =                                                     // six
        Map.find 6 patternsByCount
        |> findUniqueIntersectionOfCount 1 cf

    let f = Set.intersect abdefg cf
    let c = Set.difference cf f
    let a = Set.difference acf (Set.union c f)
    let acdfg =                                                      // three
        Map.find 5 patternsByCount
        |> findUniqueIntersectionOfCount 2 cf

    let bcdf = Map.find 4 patternsByCount |> List.head               // four
    let dg = Set.difference acdfg (Set.unionMany [a; c; f;])
    let d = Set.intersect dg bcdf
    let g = Set.difference dg d
    let b = Set.difference bcdf (Set.unionMany [c; d; f])

    let abcdefg = Map.find 7 patternsByCount |> List.head           // eight
    let e = Set.difference abcdefg (Set.unionMany [a; b; c; d; f; g;])

    let toChar set = set |> Set.toList |> unwrapSingleton |> Option.get
    let chars = List.map toChar [a; b; c; d; e; f; g;]
    List.zip chars ['a' .. 'g'] |> Map.ofList


let patternToDigit mapping pattern =
    let correctPattern =
        pattern
        |> Set.toArray
        |> Array.map (fun c -> Map.find c mapping)
        |> Array.sort
        |> String

    match correctPattern with
    | "abcefg" -> Some "0"
    | "cf" -> Some "1"
    | "acdeg" -> Some "2"
    | "acdfg" -> Some "3"
    | "bcdf" -> Some "4"
    | "abdfg" -> Some "5"
    | "abdefg" -> Some "6"
    | "acf" -> Some "7"
    | "abcdefg" -> Some "8"
    | "abcdfg" -> Some "9"
    | _ -> None


let decipher (uniquePatterns, outputValues) =
    let mapping = deduceWirePattern uniquePatterns
    outputValues
    |> List.map (patternToDigit mapping)
    |> List.map Option.get
    |> String.concat ""
    |> int


let part2 notes =
    notes
    |> List.map decipher
    |> List.sum


[<EntryPoint>]
let main argv =
    let notes =
        File.ReadAllLines(argv.[0])
        |> Seq.map(parseNote)
        |> Seq.toList

    printfn "Part 1: %d" (part1 notes)
    printfn "Part 2: %d" (part2 notes)
    0
