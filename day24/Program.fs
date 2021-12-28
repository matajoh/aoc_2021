open System.IO


type Value =
    | Literal of int64
    | W
    | X
    | Y
    | Z
    | Input of int64
    | Add of Value * Value
    | Multiply of Value * Value
    | Divide of Value * Value
    | Modulo of Value * Value
    | Equal of Value * Value


type Instruction =
    | ReadOp of Value
    | AddOp of Value * Value
    | MultiplyOp of Value * Value
    | DivideOp of Value * Value
    | ModuloOp of Value * Value
    | EqualOp of Value * Value


type State = {W: Value; X: Value; Y: Value; Z: Value;
              InputIndex: int}


let parseValue value =
    match value with
    | "w" -> W
    | "x" -> X
    | "y" -> Y
    | "z" -> Z
    | _ -> Literal (int64 value)


let parseInstruction (line : string) =
    match line.Split ' ' with
    | [| "inp"; reg |] ->
        Some (ReadOp (parseValue reg))
    | [| "add"; reg; value; |] ->
        Some (AddOp ((parseValue reg), (parseValue value)))
    | [| "mul"; reg; value; |] ->
        Some (MultiplyOp ((parseValue reg), (parseValue value)))
    | [| "div"; reg; value; |] ->
        Some (DivideOp ((parseValue reg), (parseValue value)))
    | [| "mod"; reg; value; |] ->
        Some (ModuloOp ((parseValue reg), (parseValue value)))
    | [| "eql"; reg; value; |] ->
        Some (EqualOp ((parseValue reg), (parseValue value)))
    | _ -> None


let read register state inputs =
    let value = List.item state.InputIndex inputs
    let newIndex = state.InputIndex + 1
    let someState =
        match register with
        | W -> Some {state with W=Literal value; InputIndex=newIndex}
        | X -> Some {state with X=Literal value; InputIndex=newIndex}
        | Y -> Some {state with Y=Literal value; InputIndex=newIndex}
        | Z -> Some {state with Z=Literal value; InputIndex=newIndex}
        | _ -> None
    
    Option.get someState


let rec add a b =
    match a, b with
    | Literal 0L, y -> y
    | x, Literal 0L -> x
    | Add (x, Literal y), Literal z -> Add (x, Literal (y + z))
    | Literal x, Literal y -> Literal (x + y)
    | x, y -> Add (x, y)


let rec multiply a b =
    match a, b with
    | Literal x, Literal y -> Literal (x * y)
    | x, y -> Multiply (x, y)


let rec divide a b =
    match a, b with
    | Literal x, Literal y ->
        Literal (int64(float(x) / float(y)))
    | x, y -> Divide (x, y)


let rec modulo a b =
    match a, b with
    | Literal x, Literal y -> Literal (x % y)
    | x, y -> Modulo (x, y)


let equal a b =
    match a, b with
    | Literal x, Literal y -> Literal (if x = y then 1L else 0L)
    | Input _, Literal y when y > 9 || y = 0 -> Literal 0L
    | Input _, Add (_, Literal y) when y > 9 -> Literal 0L
    | x, y -> Equal (x, y)


let load state value =
    let register =
        match value with
        | W -> Some state.W
        | X -> Some state.X
        | Y -> Some state.Y
        | Z -> Some state.Z
        | Literal x -> Some (Literal x)
        | Input x -> Some (Input x)
        | _ -> None
    
    Option.get register


let save state loc value =
    let maybeState =
        match loc with
        | W -> Some {state with W=value}
        | X -> Some {state with X=value}
        | Y -> Some {state with Y=value}
        | Z -> Some {state with Z=value}
        | _ -> None

    Option.get maybeState


let rec valueToString value =
    match value with
    | Literal value -> sprintf "%d" value
    | Input index -> sprintf "x%d" index
    | Add (a, b) -> sprintf "%s + %s" (valueToString a) (valueToString b)
    | Equal (a, b) -> sprintf "%s = %s" (valueToString a) (valueToString b)
    | _ -> ""


let execute inputs state instruction =
    match instruction with
    | ReadOp reg ->
        read reg state inputs
    | AddOp (reg, value) ->
        save state reg (add (load state reg) (load state value))
    | MultiplyOp (reg, value) ->
        save state reg (multiply (load state reg) (load state value))
    | DivideOp (reg, value) ->
        save state reg (divide (load state reg) (load state value))
    | ModuloOp (reg, value) ->
        save state reg (modulo (load state reg) (load state value))
    | EqualOp (reg, value) ->
        save state reg (equal (load state reg) (load state value))


let runProgram state inputs instructions =
    List.fold (execute inputs) state instructions


let rec runDecompiled stack iteration conditions vals =
    match vals with
    | [] -> conditions
    | (doPop, xval, yval) :: tail -> 
        let w = Input iteration
        let x = 
            match List.tryHead stack with
            | None -> xval
            | Some value -> add value xval
        let newStack = if doPop then List.tail stack else stack
        match equal w x with
        | Literal 0L ->
            runDecompiled ((add w yval)::newStack) (iteration + 1L) conditions tail
        | value ->
            let newConditions = value :: conditions
            runDecompiled newStack (iteration + 1L) newConditions tail    


let rec inputsToString inputs =
    match inputs with
    | [] -> ""
    | head :: tail -> head.ToString() + (inputsToString tail)


[<EntryPoint>]
let main argv =
    let pops = [false; false; false; false; false; true; false;
                true; true; false; true; true; true; true]
    let xvals = [10; 12; 13; 13; 14; -2; 11;
                 -15; -10; 10; -10; -4; -1; -1]
    let yvals = [0; 6; 4; 2; 9; 1; 10;
                 6; 4; 6; 3; 9; 15; 5]

    let vals =
        List.zip3 pops xvals yvals
        |> List.map (fun (a, b, c) -> a, Literal b, Literal c)

    printfn "Conditions:"
    let conditions = runDecompiled [] 0 [] vals
    for condition in conditions do
        printfn "%s" (valueToString condition)

    let instructions =
        File.ReadAllLines argv.[0]
        |> Seq.map (parseInstruction >> Option.get)
        |> Seq.toList

    let state = {
        W = Literal 0;
        X = Literal 0;
        Y = Literal 0;
        Z = Literal 0;
        InputIndex = 0;
    }

    let part1 = [9L; 4L; 9L; 9L; 2L; 9L; 9L;
                 4L; 1L; 9L; 5L; 9L; 9L; 8L]    
    let part1State = runProgram state part1 instructions
    assert (part1State.Z = Literal 0)
    printfn "Part 1: %s" (inputsToString part1)
    
    let part2 = [2L; 1L; 1L; 9L; 1L; 8L; 6L;
                 1L; 1L; 5L; 1L; 1L; 6L; 1L]
    let part2State = runProgram state part2 instructions
    assert (part2State.Z = Literal 0)
    printfn "Part 2: %s" (inputsToString part2)

    0
