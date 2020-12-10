module AdventOfCode2020.Day10

open System
open System.IO

type Joltable =
    | Outlet of Output:int
    | Adapter of MinInput:int * MaxInput:int * Output:int
    | Device of MinInput:int * MaxInput:int

let parseAdapter text = 
    let value = int text
    Adapter(MinInput = value - 3, MaxInput = value - 1, Output = value)

let parseAdapters lines =
    [
        for line in lines do
            yield parseAdapter line
    ]

let readFile =
    File.ReadAllLines
    >> parseAdapters

let maxJoltageDifference = 
    List.choose (fun jolt -> 
        match jolt with
        | Outlet (output) | Adapter (_, _, output) -> Some output
        | Device (_, maxInput) -> Some maxInput)
    >> List.max

let useAllAdapters adapters = 
    let maxAdapterOutput = maxJoltageDifference adapters
    let outlet = Outlet(Output = 0)
    let device = Adapter(MinInput = maxAdapterOutput, MaxInput = maxAdapterOutput + 3, Output = maxAdapterOutput + 3)

    [outlet]
    @ List.sortBy (fun joltable -> 
        match joltable with
        | Adapter (_, _, output) -> Some output
        | _ -> None) adapters 
    @ [device]

let aggregator difference aggregation joltable = 
    match joltable with
    | Outlet output -> (output, 0)
    | Adapter (_, _, output) -> if output - (fst aggregation) = difference then (output, (snd aggregation) + 1) else (output, snd aggregation)
    | Device (_, maxInput) -> (maxInput, (snd aggregation) + 1)
    

let countJoltageDifferences difference joltables = 
    List.scan (aggregator difference) (0, 0) joltables
    |> List.last
    |> snd

let analyzeJoltageDifferences joltables =
    let differential1 = countJoltageDifferences 1 joltables
    let differential2 = countJoltageDifferences 2 joltables
    let differential3 = countJoltageDifferences 3 joltables
    [differential1; differential2; differential3]

let findAnswer (counts:list<int>) = counts.[0] * counts.[2]

[<EntryPoint>]
let main argv =
    readFile argv.[0]
    |> useAllAdapters
    |> analyzeJoltageDifferences
    |> findAnswer
    |> printfn "The answer to part 1 is '%d'."
    
    0
