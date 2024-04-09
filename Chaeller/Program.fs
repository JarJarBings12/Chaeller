module program

open pda
open vis

let run (args: string array) handler =
    let expression = parseExpression args[1]
    match (validateExpression expression) with
    | [] -> 
        let result = runPda expression handler
        match result with
        | Value v -> printfn "'%s' = %d" args[1] v
        | Error e -> 
            printfn "Failed to evaluate expression:\n%s" e 
    | errors ->
        printfn "Invalid Expression: %s" args[1]
        printfn 
            "                     %s"
            ((expression.Length::errors, 0) 
            ||>Seq.mapFoldBack (
                fun state x -> 
                    let res = x - state
                    (res, state + x)
                )
            |> fst
            |> Seq.rev
            |> Seq.pairwise
            |> Seq.map (fun (a,b) -> $"""{String.replicate (max 0 (b - a)) " "}^""") 
            |> String.concat "")

[<EntryPoint>]
let main args =
    let emptyReport = (fun _ _ -> ())
    match args[0] with
    | "step" when args.Length > 2 -> run args displayReport
    | "nop" when args.Length > 2 -> run args emptyReport
    | "test" ->
        printfn "3 4 + 6 2 + 8 9 + 4 3 + * * *"
        run [| "nop"; "3 4 + 6 2 + 8 9 + 4 3 + * * *" |] emptyReport
        printfn "3 1 + 7 8 + 9 8 7 + 1 2 1 4 + + 7 + + + + + +"
        run [| "nop"; "3 1 + 7 8 + 9 8 7 + 1 2 1 4 + + 7 + + + + + +" |] emptyReport
        printfn "3 4 + *"
        run [| "nop"; "3 4 + *" |] emptyReport
        printfn "8 + 9 + 7 * 2 *"
        run [| "nop"; "8 + 9 + 7 * 2 *" |] emptyReport
    | arg ->
        printfn "Unknown command %s" arg
        printfn "> chaeller [nop|step] <expression>"
        printfn "> chaeller test"
    0     