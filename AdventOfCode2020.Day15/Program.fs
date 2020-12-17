namespace AdventOfCode2020.Day15

open System
open System.IO

module Program = 

    let readFile filePath = 
        (File.ReadAllText filePath).Split ','
        |> Array.toList
        |> List.map (uint64)
        |> List.rev

    let ageOfNumber spokenNumbers number = 
        if List.tail spokenNumbers |> List.contains number then
            List.findIndex (fun n -> n = number) (List.tail spokenNumbers)
            |> (+) 1
            |> uint64
        else
            0UL

    let speakNextNumber spokenNumbers = 
        List.head spokenNumbers
        |> ageOfNumber spokenNumbers
        |> (fun nextNumber -> nextNumber :: spokenNumbers)

        

    [<EntryPoint>]
    let main argv =
        let mutable spokenNumbers = readFile argv.[0]
        let target = int argv.[1]

        while (List.length spokenNumbers) < target do
            spokenNumbers <- speakNextNumber spokenNumbers

        printfn "The answer to part 1 is '%d'." (List.head spokenNumbers)

        0 