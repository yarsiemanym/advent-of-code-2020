namespace AdventOfCode2020.Day07

open System.IO
open AdventOfCode2020.Day07.Rule

module Program = 

    let readFile =
        File.ReadAllLines
        >> parseRules

    [<EntryPoint>]
    let main argv =
        let rules = readFile argv.[0]

        findValidOuterBags "shiny gold" rules
        |> Set.count
        |> printfn "The answer to part 1 is '%d'."

        countRequiredInnerBags "shiny gold" rules
        |> printfn "The answer to part 2 is '%d'."

        0
