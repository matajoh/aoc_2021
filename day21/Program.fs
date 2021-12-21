open System.IO


type Die =
    | Deterministic of int * int
    | Dirac of Map<int,uint64>


type State = {
    Score1 : int;
    Space1 : int;
    Score2 : int;
    Space2 : int;
    Phase : bool
}


let DiracMoves = 
    [
        for i = 1 to 3 do
            for j = 1 to 3 do
                for k = 1 to 3 do
                    yield i + j + k
    ]
    |> List.countBy id
    |> List.map (fun (a, b) -> a, uint64(b))
    |> Map.ofList          


let roll die =
    match die with
    | Deterministic (value, maxValue) ->
        let rollSum = value + (value + 1) % maxValue + (value + 2) % maxValue
        let moves = Map.ofList [rollSum + 3, 1UL]
        Deterministic ((value + 3) % maxValue, maxValue), moves
    | Dirac moves -> Dirac moves, moves


let update winningScore state moves =
    match state with
    | {Score1=score} when score >= winningScore -> state
    | {Score2=score} when score >= winningScore -> state
    | {Phase=phase} when phase ->
        let space = (state.Space1 + moves - 1) % 10 + 1
        let score = state.Score1 + space
        {state with Space1 = space; Score1 = score; Phase=false}
    | _ ->
        let space = (state.Space2 + moves - 1) % 10 + 1
        let score = state.Score2 + space
        {state with Space2 = space; Score2 = score; Phase=true}


let updateState winningScore moves (state, count) =
    Map.toList moves
    |> List.map (fun (move, moveCount) ->
        update winningScore state move, count * moveCount)


let rec playGame winningScore die state rolls =
    match state with
    | {Score1=score} when score >= winningScore -> state.Score2, rolls
    | {Score2=score} when score >= winningScore -> state.Score1, rolls
    | _ ->
        let newDie, moves = roll die
        let newState = 
            updateState winningScore moves (state, 1UL)
            |> List.head
            |> fst
        playGame winningScore newDie newState (rolls + 3)


let part1 (space1, space2) =
    let initState = {Space1=space1; Score1=0; Space2=space2; Score2=0; Phase=true}
    let losingScore, numRolls = playGame 1000 (Deterministic (0, 100)) initState 0
    losingScore * numRolls


let gameOver winningScore (state, _) =
    state.Score1 >= winningScore || state.Score2 >= winningScore
let win1 (state, count) = if state.Score1 > state.Score2 then count else 0UL
let win2 (state, count) = if state.Score2 > state.Score1 then count else 0UL


let rec playAllGames winningScore die (wins1, wins2) states =
    let endStates, continueStates = List.partition (gameOver winningScore) states

    let newWins1 = wins1 + (List.sumBy win1 endStates)
    let newWins2 = wins2 + (List.sumBy win2 endStates)
    if List.length continueStates = 0 then
        newWins1, newWins2
    else
        let newDie, moves = roll die
        let newStates =
            List.collect (updateState winningScore moves) continueStates
            |> List.groupBy fst
            |> List.map (fun (state, counts) -> state, counts |> List.sumBy snd)
        playAllGames winningScore newDie (newWins1, newWins2) newStates


let part2 (space1, space2) =
    let initState = {Space1=space1; Score1=0; Space2=space2; Score2=0; Phase=true}
    let player1Wins, player2Wins = playAllGames 21 (Dirac DiracMoves) (0UL, 0UL) [initState, 1UL]    
    if player1Wins > player2Wins then
        player1Wins
    else
        player2Wins


[<EntryPoint>]
let main argv =
    let spaces =
        File.ReadAllLines argv.[0]
        |> Seq.map (fun line -> line.Split ' ' |> Array.last |> int)
        |> Seq.pairwise
        |> Seq.head

    printfn "Part 1: %d" (part1 spaces)
    printfn "Part 2: %d" (part2 spaces)
    0
