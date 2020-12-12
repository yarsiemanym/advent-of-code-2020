namespace AdventOfCode2020.Day11

open System
open System.IO
open AdventOfCode2020.Day11.Space
open AdventOfCode2020.Day11.Map

module Program =

    let parseSpace symbol = 
        match symbol with
        | '.' -> Floor
        | 'L' -> Seat(Occupied = false)
        | '#' -> Seat(Occupied = true)
        | _ -> failwithf "Invalid space '%c'" symbol

    let parseSpaces rows =
        {
            Spaces = 
            [
                for row in rows do
                    [
                        for symbol in row do
                            yield parseSpace symbol
                    ]
            ]
        }

    let readFile =
        File.ReadAllLines
        >> parseSpaces

    let countOccupiedAdjacentSeats (map:Map) = map.CountOccupiedAdjacentSeats

    let countOccupiedVisibleSeats (map:Map) = map.CountOccupiedVisibleSeats

    [<EntryPoint>]
    let main argv =
        let map = readFile argv.[0]
        
        map.RunSimulation countOccupiedAdjacentSeats 4
        |> printfn "The answer to part 1 is '%d'."

        map.RunSimulation countOccupiedVisibleSeats 5
        |> printfn "The answer to part 2 is '%d'."

        0