open System
open System.IO

type Command =
    | Forward of units: int
    | Down of units: int
    | Up of units: int


type Position = { x: int; z: int; aim: int }


let parseCommand (parts: String array) =
    let units = parts.[1] |> int
    match parts.[0] with
    | "forward" -> Some (Forward(units))
    | "down" -> Some (Down(units))
    | "up" -> Some (Up(units))
    | _ -> None


let applyCommand1 pos command =
    match command with
    | Forward(command) -> { pos with x = pos.x + command }
    | Down(command) -> { pos with z = pos.z + command }
    | Up(command) -> { pos with z = pos.z - command }


let applyCommand2 pos command =
    match command with
    | Forward(command) -> { pos with x = pos.x + command; z = pos.z + command * pos.aim}
    | Down(command) -> { pos with aim = pos.aim + command }
    | Up(command) -> { pos with aim = pos.aim - command }


let move commands applyCommand =
    let pos = List.fold applyCommand { x=0; z=0; aim=0 } commands
    pos.x * pos.z


[<EntryPoint>]
let main argv =
    let commands = 
        File.ReadLines(argv.[0])
        |> Seq.map(fun line -> line.Split ' ')
        |> Seq.map(parseCommand)
        |> Seq.map(Option.get)
        |> Seq.toList
    
    printfn "Part 1: %i" (move commands applyCommand1)
    printfn "Part 2: %i" (move commands applyCommand2)
    0
