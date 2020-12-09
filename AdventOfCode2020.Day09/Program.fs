module AdventOfCode2020.Day09

open System.IO

let readFile = 
    File.ReadAllLines
    >> Array.map int64
    >> Array.toList

let isValid number (preamble:list<int64>) = 
    let mutable isValid = false
    for i in 0 .. preamble.Length - 1 do
        for j in 0 .. preamble.Length - 1 do
            if i <> j && preamble.[i] + preamble.[j] = number then
                isValid <- true
    isValid

let validateNumbers preambleLength (numbers:list<int64>) = 
    seq {
        for i in preambleLength .. numbers.Length - 1 do
            let number = numbers.[i]
            let preamble = numbers.[i - preambleLength - 1 .. i - 1]
            let vaildity = isValid number preamble
            (number, vaildity)
    }

let findInvalidNumber preambleLength =
    validateNumbers preambleLength
    >> Seq.filter (not << snd)
    >> Seq.map fst
    >> Seq.head

let aggregate agg number = ((fst agg) @ [number], (snd agg) + number)

let findContiguousSums (numbers:list<int64>) target = 
    seq {
        for i in 0 .. numbers.Length - 1 do
            let contiguousNumbers = numbers.[i .. numbers.Length - 1]
            let initialState = (List.Empty, 0L)
            let candidates = List.scan aggregate initialState contiguousNumbers

            for candidate in candidates do
                if snd candidate = target then
                    yield fst candidate
    }

let calcXmasWeakness (numbers:list<int64>) = (List.min numbers) + (List.max numbers)

let printAnswer answer = printfn "The answer is '%d'." answer

let findAnswer preambleLength filePath = 
    let numbers = readFile filePath
    
    findInvalidNumber preambleLength numbers
    |> findContiguousSums numbers
    |> Seq.head
    |> calcXmasWeakness
    |> printAnswer

[<EntryPoint>]
let main argv =
    findAnswer (int argv.[0]) argv.[1]
    0
