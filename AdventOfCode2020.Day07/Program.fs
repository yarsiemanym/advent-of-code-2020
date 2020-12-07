module AdventOfCode2020.Day07

open System
open System.IO
open System.Text.RegularExpressions

type Contents =
    {
        InnerBagColor:string
        Quantity:int
    }

type Rule =
    {
        OuterBagColor:string
        Contents:Set<Contents>
    }

let parseInnerBag text = 
    let pattern = @"^ (\d+) (.+) bags?[,.]$"
    let m = Regex.Match(text, pattern)

    if m.Success then
        {
            InnerBagColor = m.Groups.Item(2).Value
            Quantity = int(m.Groups.Item(1).Value)
        }
    else
        raise (Exception (sprintf "Invalid contents '%s'" text))


let parseInnerBags text = 
    let pattern = @"^( [^,.]*[,.])+$"
    let m = Regex.Match(text, pattern)

    Set.ofList
        [
            for capture in m.Groups.Item(1).Captures do
                if capture.Value <> " no other bags." then
                    yield parseInnerBag capture.Value
        ]

let parseRule text =
    let pattern = @"^(.*) bags contain(.*)$"
    let m = Regex.Match(text, pattern)

    if m.Success then
        {
            OuterBagColor = m.Groups.Item(1).Value
            Contents = parseInnerBags (m.Groups.Item(2).Value)
        }
    else
        raise (Exception (sprintf "Invalid rule '%s'" text))

let parseRules lines =
    Set.ofList
        [
            for line in lines do
                parseRule line
        ]

let readFile =
    File.ReadAllLines
    >> parseRules

let hasInnerBag (innerBagColor, rule) =
    Set.map (fun r -> r.InnerBagColor) rule.Contents
    |> Set.contains innerBagColor

let rec findValidOuterBags innerBagColor rules =
    Set.filter (fun r -> hasInnerBag (innerBagColor, r)) rules
    |> Set.map (fun r -> Set.union (Set.ofList [r.OuterBagColor]) (findValidOuterBags r.OuterBagColor rules))
    |> Set.unionMany

let rec countRequiredInnerBags outerBagColor rules =
    Set.filter (fun r -> r.OuterBagColor = outerBagColor) rules
    |> Set.map (fun r -> r.Contents)
    |> Set.unionMany
    |> Set.map (fun c -> c.Quantity * ((countRequiredInnerBags c.InnerBagColor rules) + 1))
    |> Set.toList
    |> List.sum

let printAnswer answer = printfn "The answer is '%d'." answer

let findAnswer = 
    readFile
    //>> findValidOuterBags "shiny gold"
    //>> Set.count
    >> countRequiredInnerBags "shiny gold"
    >> printAnswer

[<EntryPoint>]
let main argv =
    findAnswer argv.[0]
    0
