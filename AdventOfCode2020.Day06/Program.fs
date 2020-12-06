module AdventOfCode2020.Day06

open System
open System.IO
open System.Text.RegularExpressions

let processGroup text = 
    Regex.Split(text, @"\s+", RegexOptions.Singleline) 
    |> Array.filter (not << String.IsNullOrWhiteSpace)
    |> Array.toList
    |> List.map Set.ofSeq
    //|> List.reduce (Set.union)
    |> List.reduce (Set.intersect)

let processFlight text = 
    Regex.Split(text, @"(\s*\n){2,}", RegexOptions.Singleline) 
    |> Array.filter (not << String.IsNullOrWhiteSpace)
    |> Array.toList
    |> List.map processGroup

let countYesAnswers = 
    List.map Set.count
    >> List.sum

let printAnswer answer = printfn "The answer is '%d'." answer
    
let findAnswer =
    File.ReadAllText
    >> processFlight
    >> countYesAnswers
    >> printAnswer

[<EntryPoint>]
let main argv =
    findAnswer argv.[0]
    0
