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


let rec toInt bits value =
    match bits with
    | [] -> value
    | On :: tail -> toInt tail (value * 2 + 1)
    | Off :: tail -> toInt tail (value * 2)


let rec toUInt64 bits value =
    match bits with
    | [] -> value
    | On :: tail -> toUInt64 tail (value * 2UL + 1UL)
    | Off :: tail -> toUInt64 tail (value * 2UL)


type Packet =
    | LiteralPacket of version: int * value: uint64
    | OperatorPacket of version: int * op : PacketType * packets : Packet list


let rec parseLiteral bits literal =
    match bits with
    | [] -> toUInt64 literal 0UL, []
    | On :: tail -> parseLiteral (List.skip 4 tail) (literal @ (List.take 4 tail))
    | Off :: tail ->
        let value = toUInt64 (literal @ List.take 4 tail) 0UL
        let remaining = List.skip 4 tail
        value, remaining


let rec parsePacket bits parsePackets =
    if List.length bits > 7 then
        let version = toInt (List.take 3 bits) 0
        let packetType = toInt (List.take 3 (List.skip 3 bits)) 0 |> toPacketType
        let remaining = List.skip 6 bits
        match packetType with
        | None ->
            let value, remaining = parseLiteral remaining []
            Some (LiteralPacket(version, value)), remaining
        | Some op ->
            match List.head remaining with
            | On ->
                let numPackets = toInt (List.take 11 (List.tail remaining)) 0
                let tail = List.skip 12 remaining
                let packets, ending = parsePackets tail numPackets []
                Some (OperatorPacket(version, op, packets)), ending
            | Off ->
                let bitLength = toInt (List.take 15 (List.tail remaining)) 0
                let tail = List.skip 16 remaining
                let subsequence = List.take bitLength tail
                let packets, _ = parsePackets subsequence -1 []
                Some (OperatorPacket(version, op, packets)), List.skip bitLength tail
    else
        None, []


let rec parsePackets bitStream count packets =
    match bitStream, count with
    | _, 0 -> List.rev packets, bitStream
    | _ ->
        let maybePacket, remaining = parsePacket bitStream parsePackets
        match maybePacket with
        | Some packet -> parsePackets remaining (count - 1) (packet :: packets)
        | None -> List.rev packets, remaining


let rec part1 packets sum =
    match packets with
    | [] -> sum
    | LiteralPacket(version, _) :: tail -> part1 tail (sum + version)
    | OperatorPacket(version, _, subpackets) :: tail -> part1 tail (part1 subpackets (sum + version))


let apply op calculate packets = packets |> calculate |> op
let compare op calculate packets =
    match List.take 2 (packets |> calculate) with
    | [a; b] when op a b -> 1UL
    | _ -> 0UL
let product = List.fold (*) 1UL


let rec calculate packets =
    match packets with
    | [] -> []
    | head :: tail ->
        match head with
        | LiteralPacket(_, value) -> [value] @ calculate tail
        | OperatorPacket(_, Sum, subpackets) -> [apply List.sum calculate subpackets] @ calculate tail
        | OperatorPacket(_, Product, subpackets) -> [apply product calculate subpackets] @ calculate tail
        | OperatorPacket(_, Minimum, subpackets) -> [apply List.min calculate subpackets] @ calculate tail
        | OperatorPacket(_, Maximum, subpackets) -> [apply List.max calculate subpackets] @ calculate tail
        | OperatorPacket(_, GreaterThan, subpackets) -> [compare (>) calculate subpackets] @ calculate tail
        | OperatorPacket(_, LessThan, subpackets) -> [compare (<) calculate subpackets] @ calculate tail
        | OperatorPacket(_, EqualTo, subpackets) -> [compare (=) calculate subpackets] @ calculate tail


let part2 packets =
    List.head (calculate packets)


[<EntryPoint>]
let main argv =
    let bitStream =
        File.ReadAllText argv.[0]
        |> Seq.toList
        |> List.collect (toBits >> Option.get)

    let packets, _ = parsePackets bitStream -1 []
    printfn "Part 1: %d" (part1 packets 0)
    printfn "Part 2: %d" (part2 packets)
    0
