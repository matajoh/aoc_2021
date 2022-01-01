open System
open System.IO
open System.Collections.Generic


type Amphipod =
    | A
    | B
    | C
    | D


type Space =
    | Start
    | Hallway
    | Opening
    | Room of Amphipod


let Hallway = [1, 1; 1, 2; 1, 4; 1, 6; 1, 8; 1, 10; 1, 11] |> Set.ofList
let Openings = [1, 3; 1, 5; 1, 7; 1, 9] |> Set.ofList
let room expanded amphipod =
    let col =
        match amphipod with
        | A -> 3
        | B -> 5
        | C -> 7
        | D -> 9
    let maxRow = if expanded then 5 else 3
    [
        for row = 2 to maxRow do
            yield row, col
    ] |> Set.ofList

let Rooms = List.map (fun a -> a, room false a) [A; B; C; D] |> Map.ofList
let ExpandedRooms = List.map (fun a -> a, room true a) [A; B; C; D] |> Map.ofList


let parse row col char =
    match char with
    | 'A' -> Some (A, (row, col))
    | 'B' -> Some (B, (row, col))
    | 'C' -> Some (C, (row, col))
    | 'D' -> Some (D, (row, col))
    | _ -> None


let neighbors (row, col) =
    seq {row - 1, col; row + 1, col; row, col - 1; row, col + 1}


let rec findPath amphipods length (row0, col0) (row1, col1) =
    if row0 = row1 && col0 = col1 then
        Some length
    else
        let somePoint = 
            match row0, row1 with
            | 1, _ ->
                match col1 - col0 with
                | dc when dc > 0 -> Some (1, col0 + 1)
                | dc when dc < 0 -> Some (1, col0 - 1)
                | _ -> Some (row0 + 1, col0)
            | _, 1 -> Some (row0 - 1, col0)
            | _ when row0 > 1 && row1 > row0 -> Some (row0 + 1, col0)
            | _ -> None

        let point = Option.get somePoint
        match Map.tryFind point amphipods with
        | Some _ -> None
        | None -> findPath amphipods (length + 1) point (row1, col1)


let validPaths amphipods start targets =
    let amphipod = Map.find start amphipods
    targets
    |> Seq.map (fun finish -> finish, findPath amphipods 0 start finish)
    |> Seq.filter (snd >> Option.isSome)
    |> Seq.map (fun (finish, length) -> (start, finish), amphipod, Option.get length)
    |> Seq.toList


type State = {
    A : ((int * int) * int) list
    B : ((int * int) * int) list
    C : ((int * int) * int) list
    D : ((int * int) * int) list
    Expanded : bool
}


let toSpaces state =
    let toSpace a x = fst x, a
    let a = state.A |> List.map (toSpace A)
    let b = state.B |> List.map (toSpace B)
    let c = state.C |> List.map (toSpace C)
    let d = state.D |> List.map (toSpace D)
    a @ b @ c @ d |> Map.ofList


let canMove state =
    state.A @ state.B @ state.C @ state.D
    |> List.filter (fun (_, count) -> count < 2)
    |> List.map fst


let getValidRoom amphipods amphipod expanded =
    let spaces =
        if expanded then
            Map.find amphipod ExpandedRooms
        else
            Map.find amphipod Rooms

    let notSame point =
        match Map.tryFind point amphipods with
        | Some b -> amphipod <> b
        | None -> false

    let isOpen point =
        match Map.tryFind point amphipods with
        | Some _ -> false
        | None -> true

    if Seq.exists notSame spaces then
        Set.empty
    else
        let openSpaces = Seq.filter isOpen spaces |> Seq.toList
        if List.length openSpaces > 0 then
            Set.singleton (List.maxBy (fun (row, _) -> row) openSpaces)
        else
            Set.empty


let getAmphipodMoves amphipods expanded (row, col) =
    let amphipod = Map.find (row, col) amphipods
    
    let spaces =
        if row = 1 then
            getValidRoom amphipods amphipod expanded
        else
            Hallway
    
    spaces |> validPaths amphipods (row, col)


let getMoves state =
    let amphipods = toSpaces state
    canMove state
    |> Seq.collect (getAmphipodMoves amphipods state.Expanded)
    |> Seq.filter (fun (_, _, cost) -> cost > 0)
    |> Seq.toList


let rec update points start finish =
    match points with
    | [] -> []
    | (point, count) :: tail when point = start -> (finish, count + 1) :: tail
    | head :: tail -> head :: (update tail start finish)


let doMove state amphipod (start, finish) =
    match amphipod with
    | A -> {state with A = update state.A start finish}
    | B -> {state with B = update state.B start finish}
    | C -> {state with C = update state.C start finish}
    | D -> {state with D = update state.D start finish}


let toString state =
    let spaces = toSpaces state
    let getRoom a =
        if state.Expanded then
            Map.find a ExpandedRooms
        else
            Map.find a Rooms

    let openSpaces = Set.unionMany [Hallway; Openings;
                                    (getRoom A); (getRoom B);
                                    (getRoom C); (getRoom D)]

    let maxRow = if state.Expanded then 6 else 4
    [|
        for row = 0 to maxRow do
            for col = 0 to 12 do
                match Map.tryFind (row, col) spaces with
                | Some A -> yield 'A'
                | Some B -> yield 'B'
                | Some C -> yield 'C'
                | Some D -> yield 'D'
                | None ->
                    match row, col with
                    | row, col when row > 2 && col < 2 -> yield ' '
                    | row, col when row > 2 && col > 10 -> yield ' '
                    | point when Set.contains point openSpaces -> yield '.'
                    | _ -> yield '#'
            yield '\n'
    |] |> String


type AStar = {
    Start : State;
    GScore : Map<State, int>;
    OpenSet : Set<State>;
    H : (State) -> int;
    CameFrom : Map<State, State>;
    Queue : PriorityQueue<State, int>
}


let amphipodScore ((row, col), amphipod) =
    let roomCol =
        match amphipod with
        | A -> 3
        | B -> 5
        | C -> 7
        | D -> 9

    if col = roomCol then
        if row = 1 then 1 else 0
    else
        row + (abs (col - roomCol))


let heuristic state =
    toSpaces state
    |> Map.toList
    |> List.sumBy amphipodScore


let updateAStar astar current neighbor score =
    let gScore = Map.change neighbor (fun _ -> Some score) astar.GScore
    let openSet = 
        if not (Set.contains neighbor astar.OpenSet) then
            astar.Queue.Enqueue(neighbor, score + astar.H neighbor)
            Set.add neighbor astar.OpenSet
        else
            astar.OpenSet
    let cameFrom = Map.change neighbor (fun _ -> Some current) astar.CameFrom

    { astar with GScore=gScore; OpenSet=openSet; CameFrom=cameFrom}


let getScore score point =
    let maybeScore = Map.tryFind point score
    match maybeScore with
    | Some score -> score
    | None -> Int32.MaxValue


let rec addStates astar current states =
    match states with
    | [] -> astar
    | (state, distance) :: tail ->
        let tentativeGScore = (Map.find current astar.GScore) + distance
        let gScore = getScore astar.GScore state
        if tentativeGScore < gScore then
            let newAStar = updateAStar astar current state tentativeGScore
            addStates newAStar current tail
        else
            addStates astar current tail


let nextStates state =
    getMoves state
    |> List.map (fun (path, amphipod, cost) -> doMove state amphipod path, cost)


let rec rebuild astar path current =
    if astar.Start = current then
        current :: path
    else
        rebuild astar (current :: path) (Map.find current astar.CameFrom)


let rec findMinPath astar =
    let current = astar.Queue.Dequeue()
    if astar.H current = 0 then
        rebuild astar [] current
    else
        let newAStar = {astar with OpenSet = Set.remove current astar.OpenSet}
        findMinPath (addStates newAStar current (nextStates current))


let manhattan (((row0, col0), _), ((row1, col1), _)) =
    (abs (row0 - row1)) + (abs (col0 - col1))


let distance points0 points1 =
    List.zip points0 points1
    |> List.sumBy manhattan


let cost (state0, state1) =
    distance state0.A state1.A +
    10 * (distance state0.B state1.B) +
    100 * (distance state0.C state1.C) +
    1000 * (distance state0.D state1.D)


let minPath state =
    let start = state

    let queue = PriorityQueue<State, int>()
    queue.Enqueue(start, heuristic start)

    let astar = {
        Start = state;
        GScore = Map.ofList [start, 0];
        Queue = queue;
        CameFrom = Map.empty;
        H = heuristic;
        OpenSet = Set.singleton start;
    }

    let path = findMinPath astar
    for state in path do
        printfn "%s" (toString state)

    List.pairwise path
    |> List.sumBy cost


let toMoveStates points =
    points
    |> Seq.map (fun (_, point) -> point, 0)
    |> Seq.toList


let toState chars =
    chars
    |> List.groupBy fst
    |> List.map (fun (k, v) -> k, toMoveStates v)
    |> Map.ofList


let toExpandedState chars =
    let extra = [
        D, (3, 3); D, (4, 3);
        C, (3, 5); B, (4, 5);
        B, (3, 7); A, (4, 7);
        A, (3, 9); C, (4, 9);
    ]
    chars
    |> List.map (fun (amphipod, (row, col)) ->
        match row with
        | 3 -> amphipod, (5, col)
        | _ -> amphipod, (row, col))
    |> List.append extra
    |> List.groupBy fst
    |> List.map (fun (k, v) -> k, toMoveStates v)
    |> Map.ofList


let part1 amphipods =
    let state = toState amphipods
    minPath {
        A = state[A];
        B = state[B];
        C = state[C];
        D = state[D];
        Expanded = false;
    }


let part2 amphipods =
    let state = toExpandedState amphipods
    minPath {
        A = state[A];
        B = state[B];
        C = state[C];
        D = state[D];
        Expanded = true;
    }


[<EntryPoint>]
let main argv =
    let amphipods =
        File.ReadAllLines argv.[0]
        |> Seq.mapi (fun row line ->
            Seq.mapi (fun col char -> parse row col char) line)
        |> Seq.concat
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Seq.toList

    printfn "Part 1: %d" (part1 amphipods)
    printfn "Part 2: %d" (part2 amphipods)
    0
