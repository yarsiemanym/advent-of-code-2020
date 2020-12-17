namespace AdventOfCode2020.Day14

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open AdventOfCode2020.Day14.Instruction

module Program = 

    let parseInstruction line =
        let maskPattern = @"^mask = ([X10]{36})$"
        let writePattern = @"^mem\[(\d+)\] = (\d+)$"


        let maskMatch = Regex.Match(line, maskPattern)

        if Regex.IsMatch(line, maskPattern) then
            let m = Regex.Match(line, maskPattern)
            SetMask(Value = Seq.toList m.Groups.[1].Value)
        else if Regex.IsMatch(line, writePattern) then
            let m = Regex.Match(line, writePattern)
            WriteToMem(Address = int m.Groups.[1].Value, Value = uint64 m.Groups.[2].Value)
        else
            failwithf "Invalid instruction '%s'." line

    let parseInstructions lines = 
        [
            for line in lines do
                yield parseInstruction line
        ]

    let readFile = 
        File.ReadAllLines
        >> parseInstructions 

    let mutable mask:list<char> = List.Empty

    let memory = new Dictionary<int,uint64>()

    let applyMask input =
        let mutable output = input

        for i in 0 .. mask.Length - 1 do
            match mask.[i] with
            | '1' -> output <- output ||| (1UL <<< (mask.Length - 1 - i))
            | '0' -> output <- output &&& ~~~ (1UL <<< (mask.Length - 1 - i))
            | _ -> ()

        output

    let writeValue address value = 
        memory.Remove(address) |> ignore
        memory.Add(address, value)

    let execute instructions = 
        for instruction in instructions do
            match instruction with
            | SetMask(value) -> mask <- value
            | WriteToMem(address, value) -> applyMask value |> writeValue address

    let calcPart1Answer = Seq.sum

    [<EntryPoint>]
    let main argv =
        let instructions = readFile argv.[0]
        
        execute instructions

        calcPart1Answer memory.Values
        |> printfn "The answer to part 1 is '%d'."

        0