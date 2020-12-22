namespace AdventOfCode2020.Day19

open System
open System.IO
open System.Text.RegularExpressions
open AdventOfCode2020.Day19.Cache
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

    let getValidCombinations rule rules =
        let cacheEntry = checkCache rule

        if fst cacheEntry then
            snd cacheEntry
        else
            let validCombinations = rule.ValidCominations rules
            addToCache rule validCombinations
            validCombinations

    let calcPart1Answer input = 
        List.filter (fun message -> Seq.exists ((=) message) (getValidCombinations input.Rules.[0] input.Rules)) input.Messages
        |> List.length


    [<EntryPoint>]
    let main argv =
        File.ReadAllText argv.[0]
        |> parseInput
        |> calcPart1Answer
        |> printfn "The answer to part 1 is '%d'."
        
        0