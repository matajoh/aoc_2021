//inp a - Read an input value and write it to variable a.
//add a b - Add the value of a to the value of b, then store the result in variable a.
//mul a b - Multiply the value of a by the value of b, then store the result in variable a.
//div a b - Divide the value of a by the value of b, truncate the result to an integer, then store the result in variable a. (Here, "truncate" means to round the value toward zero.)
//mod a b - Divide the value of a by the value of b, then store the remainder in variable a. (This is also called the modulo operation.)
//eql a b - If the value of a and b are equal, then store the value 1 in variable a. Otherwise, store the value 0 in variable a.
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
              InputIndex: int; Conditions: (Value * bool) list}


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


let read register state =
    let index = state.InputIndex
    let newIndex = index + 1
    let someState =
        match register with
        | W -> Some {state with W=Input index; InputIndex=newIndex}
        | X -> Some {state with X=Input index; InputIndex=newIndex}
        | Y -> Some {state with Y=Input index; InputIndex=newIndex}
        | Z -> Some {state with Z=Input index; InputIndex=newIndex}
        | _ -> None
    
    [Option.get someState]


let rec add a b =
    match a, b with
    | Literal 0L, y -> y
    | x, Literal 0L -> x
    | Literal x, Literal y -> Literal (x + y)
    | Add (Input x, Literal y), Literal z -> Add (Input x, Literal (y + z))
    | Add (Literal x, Input y), Literal z -> Add (Input y, Literal (x + z))
    | Add (w, Literal x), Add (y, Literal z) ->
        Add ((add w y), (Literal (x + z)))
    | x, y -> Add (x, y)


let rec multiply a b =
    match a, b with
    | Literal 1L, y -> y
    | x, Literal 1L -> x
    | Literal 0L, _ -> Literal 0L
    | _, Literal 0L -> Literal 0L
    | Literal x, Literal y -> Literal (x * y)
    | Multiply (Input x, Literal y), Literal z -> Multiply (Input x, Literal (y * z))
    | Multiply (Literal x, Input y), Literal z -> Multiply (Input y, Literal (x * z))
    //| Add (x, y), Literal z -> Add (multiply x b, multiply y b)
    | x, y -> Multiply (x, y)


let rec divide a b =
    match a, b with
    | Literal 1L, y -> y
    | x, Literal 1L -> x
    | Literal x, Literal y ->
        Literal (int(float(x) / float(y)))
    | Divide (Input x, Literal y), Literal z ->
        Divide (Input x, Literal (y * z))
    | Divide (Literal x, Input y), Literal _ ->
        Divide (Literal x, multiply (Input y) b)
    | Multiply (Input x, Literal y), Literal z when y % z = 0L ->
        divide (Input x) (Literal (y / z))
    | x, y -> Divide (x, y)


let rec modulo a b =
    match a, b with
    | Literal x, Literal y -> Literal (x % y)
    | Modulo (_, Literal y), Literal z when y = z -> a
    | Input _, Literal y when y > 9L -> a
    | Add (Input _, Literal x), Literal y when x < y - 9L -> a
    | Add (Multiply (_, Literal x), y), Literal z when x = z -> modulo y b
    | x, y -> Modulo (x, y)


let equal a b =
    match a, b with
    | Literal x, Literal y -> Literal (if x = y then 1L else 0L)
    | Input _, Literal y when y > 9L -> Literal 0L
    | Literal x, Input _ when x > 9L -> Literal 0L
    | Add (_, Literal y), Input _ when y > 8L -> Literal 0L
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
    match value with
    | Equal (_, _) ->
        [Literal 0L, (value, true); Literal 1L, (value, false)]
        |> List.map (fun (outcome, condition) ->                
            match loc with
            | W -> Some {state with W=outcome; Conditions=condition :: state.Conditions}
            | X -> Some {state with W=outcome; Conditions=condition :: state.Conditions}
            | Y -> Some {state with W=outcome; Conditions=condition :: state.Conditions}
            | Z -> Some {state with W=outcome; Conditions=condition :: state.Conditions}
            | _ -> None)
        |> List.map Option.get
    | _ ->
        let maybeState =
            match loc with
            | W -> Some {state with W=value}
            | X -> Some {state with X=value}
            | Y -> Some {state with Y=value}
            | Z -> Some {state with Z=value}
            | _ -> None
        [Option.get maybeState]


let rec valueToString value =
    match value with
    | Literal value -> sprintf "%d" value
    | Input index -> sprintf "x%d" index
    | Add (a, b) -> sprintf "(%s + %s)" (valueToString a) (valueToString b)
    | Multiply (a, b) -> sprintf "(%s * %s)" (valueToString a) (valueToString b)
    | Divide (a, b) -> sprintf "(%s / %s)" (valueToString a) (valueToString b)
    | Modulo (a, b) -> sprintf "(%s %% %s)" (valueToString a) (valueToString b)
    | Equal (a, b) -> sprintf "(%s = %s)" (valueToString a) (valueToString b)
    | _ -> ""


let stateToString state =
    sprintf "{\n\tW=%s;\n\tX=%s;\n\tY=%s;\n\tZ=%s\n}" (valueToString state.W) (valueToString state.X) (valueToString state.Y) (valueToString state.Z)


let execute states instruction =
    for state in states do
        printfn "%s\n%A" (stateToString state) instruction

    states
    |> List.collect (fun state ->
        match instruction with
        | ReadOp reg ->
            read reg state
        | AddOp (reg, value) ->
            save state reg (add (load state reg) (load state value))
        | MultiplyOp (reg, value) ->
            save state reg (multiply (load state reg) (load state value))
        | DivideOp (reg, value) ->
            save state reg (divide (load state reg) (load state value))
        | ModuloOp (reg, value) ->
            save state reg (modulo (load state reg) (load state value))
        | EqualOp (reg, value) ->
            save state reg (equal (load state reg) (load state value)))


let runProgram state instructions =
    List.fold execute [state] instructions


[<EntryPoint>]
let main argv =
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
        Conditions = [];
    }
    let newState = runProgram state instructions
    0
