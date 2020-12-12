namespace AdventOfCode2020.Day06

open System
open System.IO
open System.Text.RegularExpressions

module Program =

    let processPassenger = Set.ofSeq

    let processGroup aggreagator text = 
        Regex.Split(text, @"\s+", RegexOptions.Singleline) 
        |> Array.toList
        |> List.filter (not << String.IsNullOrWhiteSpace)
        |> List.map processPassenger
        |> List.reduce aggreagator

    let processFlight aggregator text = 
        Regex.Split(text, @"(\s*\n){2,}", RegexOptions.Singleline) 
        |> Array.toList
        |> List.filter (not << String.IsNullOrWhiteSpace)
        |> List.map (processGroup aggregator)

    let countYesAnswers = 
        List.map Set.count
        >> List.sum

    [<EntryPoint>]
    let main argv =
        let declarations = File.ReadAllText argv.[0]

        processFlight Set.union declarations
        |> countYesAnswers
        |> printfn "The answer to part 1 is '%d'."

        processFlight Set.intersect declarations
        |> countYesAnswers
        |> printfn "The answer to part 2 is '%d'."
        0
