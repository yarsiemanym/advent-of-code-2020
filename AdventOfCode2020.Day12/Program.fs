namespace AdventOfCode2020.Day12 

open System.IO
open AdventOfCode2020.Day12.Ferry
open AdventOfCode2020.Day12.Instruction

module Program = 

    let readFile = File.ReadAllLines >> parseInstructions

    [<EntryPoint>]
    let main argv =
        let instructions = readFile argv.[0]

        let start1 = { Position = { X = 0; Y = 0 }; Heading = 'E' }
        let finish1 = start1.FollowInstructions instructions
        start1.Position.ManhattanDistance finish1.Position
        |> printfn "The answer to part 1 is '%d'."

        let start2 = { Position = { X = 0; Y = 0 }; WayPoint = { X = 10; Y = 1 } }
        let finish2 = start2.FollowInstructions instructions
        start2.Position.ManhattanDistance finish2.Position
        |> printfn "The answer to part 2 is '%d'."

        0
