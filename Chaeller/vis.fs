module vis

open System
open Spectre.Console.Rendering
open pda
open FsSpectre
open Spectre.Console

let makeStackFrame label color =
    markup {
        text $"[{color}][/]{label}"
    } :> IRenderable

let renderStack stack =
    let rec innerRenderStack stack elements =
        match stack with
        | value::xs ->
            innerRenderStack xs (makeStackFrame value "white"::elements)
        | [] -> elements
    innerRenderStack stack []

let renderStackWithChange stack changes =
    let rec innerRenderChanges changes visDisplay =
        match changes with
        | Push v::xs -> innerRenderChanges xs (makeStackFrame v "green"::visDisplay)
        | Pop v::xs -> innerRenderChanges xs (makeStackFrame v "red"::visDisplay)
        | Display::xs -> (xs, visDisplay)
        | _ -> (changes, visDisplay)
    let stack = renderStack stack
    innerRenderChanges changes stack
    


let renderOperation operation a b stack actions =
    let (actions, stackPanel) = renderStackWithChange stack actions
    grid {
        number_of_columns 2
        
        row [|
            grid {
                number_of_columns 3
                
                row [|
                    panel {
                        content_text (a |> string)
                    }
                    panel {
                        content_text (operation |> string)
                    }
                    panel {
                        content_text (b |> string)
                    }
                |]
            }
            
            panel {
                width 40
                header_text "Stack"
                content_renderable (
                    rows {
                        items_renderable (stackPanel |> Seq.toArray) 
                    }
                )
            }
        |]
    }

let displayReport stack actions =
    match actions with
    | Op sym::Pop a:: Pop b::xs ->
        let r = renderOperation sym a b stack xs
        r |> AnsiConsole.Write
    | _ ->
        printfn "Invalid"
        Console.ReadKey() |> ignore
    