namespace AdventOfCode2020.Day17

open System
open System.IO
open AdventOfCode2020.Day17.PocketDimension

module Program =

    let readFile =
        File.ReadAllLines
        >> Array.toList
        >> parsePocketDimension

    let calcPart1Answer (finalState:PocketDimension) =
        Map.toList finalState.Cubes
        |> List.map snd 
        |> List.countBy (fun cube -> cube.IsActive)
        |> List.filter fst
        |> List.map snd
        |> List.head

    [<EntryPoint>]
    let main argv =
        let initialState = readFile argv.[0]
        let mutable currentState = initialState

        for i in 0 .. 5 do
            currentState <- currentState.AdvanceBootCycle

        calcPart1Answer currentState
        |> printfn "The answer to part 1 is '%d'."

        0