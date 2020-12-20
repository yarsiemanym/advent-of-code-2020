namespace AdventOfCode2020.Day17

open System
open System.IO
open AdventOfCode2020.Day17.PocketUniverse

module Program =

    let countActiveCubes =
        Map.toList
        >> List.map snd 
        >> List.countBy (fun cube -> cube.IsActive)
        >> List.filter fst
        >> List.map snd
        >> List.head

    [<EntryPoint>]
    let main argv =
        let lines = 
            File.ReadAllLines argv.[0]
            |> Array.toList
        
        let mutable state3D = parsePocketUniverse3D lines

        for i in 0 .. 5 do
            state3D <- state3D.AdvanceBootCycle

        countActiveCubes state3D.Cubes
        |> printfn "The answer to part 1 is '%d'."

        let mutable state4D = parsePocketUniverse4D lines

        for i in 0 .. 5 do
            state4D <- state4D.AdvanceBootCycle

        countActiveCubes state4D.Cubes
        |> printfn "The answer to part 2 is '%d'."

        0