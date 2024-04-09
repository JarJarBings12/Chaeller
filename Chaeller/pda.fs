module pda

open System

type CalculationResult<'a> =
    | Value of 'a
    | Error of string

type InputValue<'a> =
    | Number of 'a
    | Operator of char * ('a -> 'a -> 'a)
    | Space
    | InvalidInput of string
 
type VisualizeAction<'a> =
    | Push of 'a
    | Pop of 'a
    | Op of char
    | Display
    
let parseExpression input =
    let handleChar c =
        match (Char.IsAsciiDigit c) with
            | true -> Number (Char.GetNumericValue c |> int64)
            | false ->
                match c with
                | '*' -> Operator ('*', (*)) 
                | '+' -> Operator ('+', (+))
                | ' ' -> Space
                | _ -> InvalidInput "Read invalid char"
    input |> Seq.map handleChar 
          |> Seq.toList

let validateExpression expression =
    let rec innerValidateExpression leftToCheck count errors =
        match leftToCheck with
        | InvalidInput _::xs ->
            innerValidateExpression xs (count+1) errors@[count]
        | _::xs -> 
            innerValidateExpression xs (count+1) errors
        | [] -> errors
    innerValidateExpression expression 0 []

[<TailCall>]
let runPda expression report =
    let doOperation sym op stack =
        match stack with
        | a::b::xs ->
            let result = op a b
            (xs, Value result, [Op sym; Pop a; Pop b; Display; Push result])
        | xs ->
            (xs, Error "Stack to small or empty", [Display])

    let rec innerRunPda expression stack =
        match expression with
        | Space::xs -> 
            innerRunPda xs stack 
        | Number x::xs ->
            report stack [Push x]
            innerRunPda xs (x::stack)
        | Operator (sym,op)::xs ->
            match (doOperation sym op stack) with
            | (stack, Value result, actions) ->
                report stack actions
                innerRunPda xs (result::stack)
            | (_, Error msg, _) ->
                Error $"Evaluation Error: {msg}"
        | InvalidInput msg::_ ->
            Error $"Invalid Expression: {msg}"
        | [] ->
            match stack with
            | [result] -> Value result
            | [] -> Error "Evaluation Error: No element on stack"
            | _ -> Error "Evaluation Error: Invalid Expression"
    innerRunPda expression []   
    

