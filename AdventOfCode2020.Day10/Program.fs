namespace AdventOfCode2020.Day10

open System.IO
open AdventOfCode2020.Day10.Joltable
open AdventOfCode2020.Day10.Cache

module Program = 

    let parseAdapter text = 
        let value = int text
        Adapter(Output = value)

    let parseAdapters lines =
        [
            for line in lines do
                yield parseAdapter line
        ]

    let readFile =
        File.ReadAllLines
        >> parseAdapters

    let maxJoltageDifference = 
        List.map (fun (joltable:Joltable) -> joltable.Output)
        >> List.max

    let addOutlet joltables =
        let outlet = Outlet
        [outlet] @ joltables

    let addDevice joltables =
        let maxAdapterOutput = maxJoltageDifference joltables
        let device = Device(MaxInput = maxAdapterOutput + 3)
        joltables @ [device]

    let useAllAdapters = List.sortBy (fun (joltable:Joltable) -> joltable.Output)

    let isBetween min max value = min <= value && value <= max

    let rec countValidAdapterChains (source:Joltable) choices = 
        let cacheEntry = checkCache source

        if fst cacheEntry then
            snd cacheEntry
        else
            let validChoices = List.filter (fun (joltable:Joltable) -> source.Output |> isBetween joltable.MinInput joltable.MaxInput) choices

            match source with
            | Device (_) -> 1UL
            | _ -> 
                let count = List.fold (fun count joltable -> count + (countValidAdapterChains joltable (List.except [joltable] choices))) 0UL validChoices
                addToCache source count
                count

    let aggregator difference aggregation (joltable:Joltable) = 
        if joltable.Output - (fst aggregation) = difference then 
            (joltable.Output, (snd aggregation) + 1) 
        else 
            (joltable.Output, snd aggregation)
        
    let countJoltageDifferences difference joltables = 
        List.scan (aggregator difference) (0, 0) joltables
        |> List.last
        |> snd

    let analyzeJoltageDifferences joltables =
        let differential1 = countJoltageDifferences 1 joltables
        let differential2 = countJoltageDifferences 2 joltables
        let differential3 = countJoltageDifferences 3 joltables
        [differential1; differential2; differential3]

    let calcPart1Answer (counts:list<int>) = counts.[0] * counts.[2]

    [<EntryPoint>]
    let main argv =
        let adapters = readFile argv.[0]

        adapters
        |> addOutlet
        |> addDevice
        |> useAllAdapters
        |> analyzeJoltageDifferences
        |> calcPart1Answer
        |> printfn "The answer to part 1 is '%d'."

        adapters
        |> addDevice
        |> countValidAdapterChains Outlet
        |> printfn "The answer to part 2 is '%d'."
        
        0