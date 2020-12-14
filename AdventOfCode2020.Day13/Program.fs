namespace AdventOfCode20202.Day13

open System.IO
open AdventOfCode2020.Day13.Bus

module Program = 

    let readFile filePath =
        let lines = File.ReadAllLines filePath
        (uint64 lines.[0], parseBuses lines.[1])

    let calcPart1Answer myArrivalTime (bus:Bus, nextDepartureTime) = bus.Id * (nextDepartureTime - myArrivalTime)

    let calcPart2AnswerSlow (buses:list<Bus>) = 
        let activeBuses = List.filter (fun (bus:Bus) -> bus.InService) buses
        let anchorBus = List.head activeBuses
        let mutable globalSyncTime = 0UL
        let mutable continueLoop = true

        while continueLoop do
            let syncTimes = [ for bus in activeBuses.Tail do anchorBus.NextSyncTime bus globalSyncTime ]
            let count = List.distinct syncTimes |> List.length
            globalSyncTime <- List.max syncTimes
            continueLoop <- count <> 1

        globalSyncTime

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
        |> calcPart2AnswerSlow
        |> printfn "The answer to part 2 is '%d'."

        0
