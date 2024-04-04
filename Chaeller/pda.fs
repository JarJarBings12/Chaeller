module pda

open System

type CalculationResult<'a> =
    | Value of 'a
    | Error of string

type InputValue<'a> =
    | Number of 'a
    | Operator of char * ('a -> 'a -> 'a)
    | InvalidInput of string
 
type VisualizeAction<'a> =
    | Push of 'a
    | Pop of 'a
    | Op of char
    | Display
    | Clear
    
let parseExpression input =
    let handleChar c =
        match (Char.IsAsciiDigit c) with
            | true -> Number (Char.GetNumericValue c |> int64)
            | false ->
                match c with
                | '*' -> Operator ('*', (*)) 
                | '+' -> Operator ('+', (+))
                | _ -> InvalidInput "Read invalid char"
                
    let rec innerParseExpression input output =
        match input with
        | [] -> output
        | x::xs -> innerParseExpression xs ((handleChar x)::output)
    innerParseExpression (input |> Seq.toList) []

let runPda expression report =
    let doOperation sym op stack =
        match stack with
        | a::b::xs ->
            let result = op a b
            (xs, Value result, [Op sym; Pop a; Pop b; Push result])
        | xs ->
            (xs, Error "Stack to small or empty", [Display])
        
    let rec innerRunPda expression stack =
        match expression with
        | Number x::xs ->
            innerRunPda xs (x::stack)
        | Operator (sym,op)::xs ->
            match (doOperation sym op stack) with
            | (stack, Value result, actions) ->
                report stack actions
                innerRunPda xs (result::stack)
            | (stack, Error msg, actions) ->
                printfn "%s" msg
        | InvalidInput msg::xs ->
            printfn "%s" msg
        | [] ->
            match stack with
            | [result] -> printfn "%s" (result |> string)
            | [] -> printfn "Invalid Expression: No element on stack"
            | _ -> printfn "?!"
    innerRunPda expression []   
    

