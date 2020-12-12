namespace AdventOfCode2020.Day08

open System
open System.IO
open System.Text.RegularExpressions
open AdventOfCode2020.Day08.Instruction

module Program =

    let readFile =
        File.ReadAllLines
        >> parseInstructionSet

    [<EntryPoint>]
    let main argv =

        readFile argv.[0]
        |> executeInstructionSet false
        |> printfn "The answer to part 1 is '%d'."

        readFile argv.[0]
        |> buildAlternateInstructionSets
        |> testInstructionSets
        |> printfn "The answer to part 2 is '%d'."
        0
