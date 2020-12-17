namespace AdventOfCode20202.Day13

open System.IO
open AdventOfCode2020.Day13.Bus

module Program = 

    let readFile filePath =
        let lines = File.ReadAllLines filePath
        (uint64 lines.[0], parseBuses lines.[1])

    let calcPart1Answer myArrivalTime (bus:Bus, nextDepartureTime) = bus.Id * (nextDepartureTime - myArrivalTime)

    let calcPart2Answer (buses:list<Bus>) =
        let mutable time = 0UL
        let mutable increment = 1UL
        
        for bus in buses do
            while (time + bus.Index) % bus.Id <> 0UL do
                time <- time + increment

            increment <- increment * bus.Id

        time

    [<EntryPoint>]
    let main argv =
        let input = readFile argv.[0]
        let myArrivalTime = fst input
        let buses = snd input
        
        buses
        |> List.filter (fun b -> b.InService)
        |> List.map (fun b -> (b, b.NextDeparture myArrivalTime))
        |> List.minBy snd
        |> calcPart1Answer myArrivalTime
        |> printfn "The answer to part 1 is '%d'."

        buses
        |> List.filter (fun b -> b.InService)
        |> calcPart2Answer
        |> printfn "The answer to part 2 is '%d'."

        0
