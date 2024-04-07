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
            innerRenderStack xs (makeStackFrame $"~ {value}" "white"::elements)
        | [] -> elements
    innerRenderStack stack [] |> List.rev

let renderStackWithChange stack changes =
    let rec renderChanges changes visDisplay =
        match changes with
        | Push v::xs -> 
            renderChanges xs (makeStackFrame $"> {v}" "green"::visDisplay)
        | Pop v::xs -> 
            renderChanges xs (makeStackFrame $"< {v}" "red"::visDisplay)
        | Display::xs -> 
            (xs, visDisplay |> List.rev)
        | _ -> 
            (changes, visDisplay |> List.rev)
    let stack = renderStack stack 
    let (changesLeft, visDisplay) = renderChanges changes []
    (changesLeft, visDisplay@stack)

let makeBaseGrid left right =
    grid {
        number_of_columns 2
        row [| left; right |]
    }
 
let makeStackPanel stackToShow =
    panel {
        header_text "Stack"
        content_renderable (
            rows {
                items_renderable (stackToShow |> Seq.toArray) 
            }
        )
    }

let makeCalcView sym a b =
    grid {
        number_of_columns 3
                    
        row [|
            panel {
                content_text (a |> string)
            }
            panel {
                content_text (sym |> string)
            }
            panel {
                content_text (b |> string)
            }
        |]
    }

let handleOperation operation a b stack actions =
    let (newActions, stackPanel) = 
        renderStackWithChange stack actions
    let innerRenderOperation showStack =
        makeBaseGrid (makeCalcView operation a b) (makeStackPanel showStack)
    innerRenderOperation stackPanel |> AnsiConsole.Write
    let (_, newStackPanel) = renderStackWithChange stack newActions
    printfn "Press [any] key to continue..."
    Console.ReadKey() |> ignore
    innerRenderOperation newStackPanel |> AnsiConsole.Write
    printfn "press [any] key to continue..."
    Console.ReadKey() |> ignore
    
let handleStackOnly stack actions =
    let (_, stackToShow) = renderStackWithChange stack actions
    makeBaseGrid (makeCalcView '-' '-' '-') (makeStackPanel stackToShow) |> AnsiConsole.Write


let displayReport stack actions =
    match actions with
    | Op sym::Pop a:: Pop b::xs ->
        handleOperation sym a b stack (Pop a:: Pop b::xs)
    | _ ->
        handleStackOnly stack actions
    