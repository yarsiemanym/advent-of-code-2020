module AdventOfCode2020.Day06

open System
open System.IO
open System.Text.RegularExpressions

let processPassenger = Set.ofSeq

let processGroup text = 
    Regex.Split(text, @"\s+", RegexOptions.Singleline) 
    |> Array.toList
    |> List.filter (not << String.IsNullOrWhiteSpace)
    |> List.map processPassenger
    //|> List.reduce (Set.union)
    |> List.reduce (Set.intersect)

let processFlight text = 
    Regex.Split(text, @"(\s*\n){2,}", RegexOptions.Singleline) 
    |> Array.toList
    |> List.filter (not << String.IsNullOrWhiteSpace)
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
