namespace AdventOfCode2020.Day18

open System
open System.IO

module Program =

    [<EntryPoint>]
    let main argv =
        
        File.ReadAllLines argv.[0]
        |> MathV1.parseExpressions
        |> List.sumBy (fun expression -> expression.Evaluate)
        |> printfn "The answer to part 1 is '%d'."

        File.ReadAllLines argv.[0]
        |> MathV2.parseExpressions
        |> List.sumBy (fun expression -> expression.Evaluate)
        |> printfn "The answer to part 2 is '%d'."
        
        0 
