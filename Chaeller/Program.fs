module program

open pda
open vis

[<EntryPoint>]
let main argv =
    if argv.Length < 2 then
        printfn "Expected two arguments: "
        printfn "> chaeller [nop|step] <expression>"
    else
        let handler =
            match argv[0] with
            | "step" -> Some displayReport
            | "nop" -> Some (fun _ _ -> ())
            | arg ->
                printfn "Invalid argument: %s" arg
                None
        
        let expression = parseExpression argv[1]
        match (validateExpression expression) with
        | [] -> 
            match handler with
            | Some handler ->
                let result = runPda expression handler
                match result with
                | Value v -> printfn "%s=%d" argv[1] v
                | Error e -> 
                    printfn "Failed to evaluate expression:\n%s" e
            | None -> ()   
        | errors ->
            printfn "Invalid Expression: %s" argv[1]
            printfn 
                "                    %s"
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
    0     