namespace AdventOfCode20202.Day13

open System.IO
open AdventOfCode2020.Day13.Bus

module Program = 

    let readFile filePath =
        let lines = File.ReadAllLines filePath
        (int lines.[0], parseBuses lines.[1])

    let calcPart1Answer myArrivalTime (bus, nextDepartureTime) = bus.Id * (nextDepartureTime - myArrivalTime)

    [<EntryPoint>]
    let main argv =
        let input = readFile argv.[0]
        let myArrivalTime = fst input
        let buses = snd input

        buses
        |> List.map (fun b -> (b, b.NextDepartureAfter myArrivalTime))
        |> List.minBy snd
        |> calcPart1Answer myArrivalTime
        |> printfn "The answer to part 1 is '%d'."

        0
