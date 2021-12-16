open System.IO

type Bit =
    | On
    | Off


let toBits hex =
    match hex with
    | '0' -> Some [Off; Off; Off; Off]
    | '1' -> Some [Off; Off; Off; On]
    | '2' -> Some [Off; Off; On; Off]
    | '3' -> Some [Off; Off; On; On]
    | '4' -> Some [Off; On; Off; Off]
    | '5' -> Some [Off; On; Off; On]
    | '6' -> Some [Off; On; On; Off]
    | '7' -> Some [Off; On; On; On]
    | '8' -> Some [On; Off; Off; Off]
    | '9' -> Some [On; Off; Off; On]
    | 'A' -> Some [On; Off; On; Off]
    | 'B' -> Some [On; Off; On; On]
    | 'C' -> Some [On; On; Off; Off]
    | 'D' -> Some [On; On; Off; On]
    | 'E' -> Some [On; On; On; Off]
    | 'F' -> Some [On; On; On; On]
    | _ -> None


type PacketType =
    | Sum
    | Product
    | Minimum
    | Maximum
    | GreaterThan
    | LessThan
    | EqualTo


let toPacketType typeID =
    match typeID with
    | 0 -> Some Sum
    | 1 -> Some Product
    | 2 -> Some Minimum
    | 3 -> Some Maximum
    | 5 -> Some GreaterThan
    | 6 -> Some LessThan
    | 7 -> Some EqualTo
    | _ -> None


let rec toInt value bits =
    match bits with
    | [] -> value
    | On :: tail -> toInt (value * 2 + 1) tail
    | Off :: tail -> toInt (value * 2) tail


let rec toUInt64 value bits =
    match bits with
    | [] -> value
    | On :: tail -> toUInt64 (value * 2UL + 1UL) tail
    | Off :: tail -> toUInt64 (value * 2UL) tail


type Packet =
    | LiteralPacket of version: int * value: uint64
    | OperatorPacket of version: int * op : PacketType * packets : Packet list

[<Literal>]
let LiteralLength = 4

let rec parseLiteral bits literal =
    match bits with
    | [] -> literal |> toUInt64 0UL, []
    | On :: tail ->
        let remaining = List.skip LiteralLength tail
        let literalBits = List.take LiteralLength tail
        parseLiteral remaining (literal @ literalBits)
    | Off :: tail ->
        let remaining = List.skip LiteralLength tail
        let value =
            literal @ List.take LiteralLength tail
            |> toUInt64 0UL
        value, remaining


[<Literal>]
let VersionLength = 3

[<Literal>]
let TypeLength = 3

[<Literal>]
let NumPacketsLength = 11

[<Literal>]
let NumBitsLength = 15


let rec parsePacket bits parsePackets =
    if List.length bits > 7 then
        let version = List.take VersionLength bits |> toInt 0
        let packetType =
            bits
            |> List.skip VersionLength
            |> List.take TypeLength
            |> toInt 0
            |> toPacketType
        let remaining = List.skip (VersionLength + TypeLength) bits
        match packetType with
        | None ->
            let value, remaining = parseLiteral remaining []
            Some (LiteralPacket(version, value)), remaining
        | Some op ->
            match List.head remaining with
            | On ->
                let numPackets =
                    List.tail remaining
                    |> List.take NumPacketsLength
                    |> toInt 0
                let packets, tail = 
                    List.skip (NumPacketsLength + 1) remaining
                    |> parsePackets numPackets []
                Some (OperatorPacket(version, op, packets)), tail
            | Off ->
                let bitLength = 
                    List.tail remaining
                    |> List.take NumBitsLength
                    |> toInt 0
                let tail = List.skip (NumBitsLength + 1) remaining
                let packets, _ = List.take bitLength tail |> parsePackets -1 []
                Some (OperatorPacket(version, op, packets)), List.skip bitLength tail
    else
        None, []


let rec parsePackets count packets bitStream =
    match bitStream, count with
    | _, 0 -> List.rev packets, bitStream
    | _ ->
        let maybePacket, remaining = parsePacket bitStream parsePackets
        match maybePacket with
        | Some packet -> parsePackets (count - 1) (packet :: packets) remaining
        | None -> List.rev packets, remaining


let rec part1 packets sum =
    match packets with
    | [] -> sum
    | LiteralPacket(version, _) :: tail ->
        part1 tail (sum + version)
    | OperatorPacket(version, _, subpackets) :: tail ->
        part1 tail (part1 subpackets (sum + version))


let compare op values =
    match List.take 2 values with
    | [a; b] when op a b -> 1UL
    | _ -> 0UL


let execute op values =
    match op with
    | Sum -> List.sum values
    | Product -> List.fold (*) 1UL values
    | Minimum -> List.min values
    | Maximum -> List.max values
    | GreaterThan -> compare (>) values
    | LessThan -> compare (<) values
    | EqualTo -> compare (=) values


let rec calculate packets =
    match packets with
    | [] -> []
    | head :: tail ->
        match head with
        | LiteralPacket(_, value) -> [value] @ calculate tail
        | OperatorPacket(_, op, subpackets) -> 
            [subpackets |> calculate |> execute op] @ calculate tail


let part2 packets =
    List.head (calculate packets)


[<EntryPoint>]
let main argv =
    let bitStream =
        File.ReadAllText argv.[0]
        |> Seq.toList
        |> List.collect (toBits >> Option.get)

    let packets, _ = parsePackets -1 [] bitStream
    printfn "Part 1: %d" (part1 packets 0)
    printfn "Part 2: %d" (part2 packets)
    0
