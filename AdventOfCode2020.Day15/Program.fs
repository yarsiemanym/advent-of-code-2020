namespace AdventOfCode2020.Day15

open System
open System.Collections.Generic
open System.IO

module Program = 

    let mutable rememberedNumbers = new Dictionary<int, int>()

    let readFile filePath = 
        (File.ReadAllText filePath).Split ','
        |> Array.toList
        |> List.map (int)

    let ageOfNumber number currentRound = 
        if rememberedNumbers.ContainsKey(number) then
            currentRound - 1 - rememberedNumbers.[number]
        else
            0

    let rememberNumber number round = 
        rememberedNumbers.Remove(number) |> ignore
        rememberedNumbers.Add(number, round)

    let initializeGame startingNumbers =
        rememberedNumbers <- new Dictionary<int, int>()
        for i in 0 .. (List.length startingNumbers) - 1 do
            let number = List.item i startingNumbers
            rememberNumber number (i + 1)

    let playGame initialState numberOfRounds =
        List.take (List.length initialState - 1) initialState
        |> initializeGame

        let mutable lastNumber = List.last initialState

        for roundNumber in initialState.Length + 1 .. numberOfRounds do
            let nextNumber = ageOfNumber lastNumber roundNumber
            rememberNumber lastNumber (roundNumber - 1)
            lastNumber <- nextNumber

        lastNumber

    [<EntryPoint>]
    let main argv =
        let mutable initialState = readFile argv.[0]

        playGame initialState 2020
        |> printfn "The answer to part 1 is '%d'."

        playGame initialState 30000000
        |> printfn "The answer to part 2 is '%d'."

        0 