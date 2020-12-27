namespace AdventOfCode2020.Day19

open System
open System.IO
open System.Text.RegularExpressions
open AdventOfCode2020.Day19.Rules

module Program = 

    type Input =
        {
            Rules:Map<int, Rule>
            Messages:list<string>
        }

    let parseInput text =
        let sections =
            Regex.Split(text, @"(\s*\n){2,}")
            |> Array.filter (not << String.IsNullOrWhiteSpace)

        {
            Rules = parseRules (sections.[0].Split('\n'))
            Messages = Array.toList (sections.[1].Split('\n'))
        }

    let findValidMessages input = 
        List.filter (fun message -> 
            let results = validate input.Rules 0 message
            List.exists ((=) message) results
        ) input.Messages

    [<EntryPoint>]
    let main argv =
        File.ReadAllText argv.[0]
        |> parseInput
        |> findValidMessages
        |> List.length
        |> printfn "The answer is '%d'."
        
        0