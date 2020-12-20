namespace AdventOfCode2020.Day18

open System
open System.IO
open AdventOfCode2020.Day18.Math

module Program =

    let readFile =
        File.ReadAllLines
        >> parseExpressions

    [<EntryPoint>]
    let main argv =
        readFile argv.[0]
        |> List.sumBy (fun expression -> expression.Evaluate)
        |> printfn "The answer to part 1 is '%d'."
        
        0 
