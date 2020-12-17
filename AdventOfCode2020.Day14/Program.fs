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
            WriteToMem(Address = uint64 m.Groups.[1].Value, Value = uint64 m.Groups.[2].Value)
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

    [<EntryPoint>]
    let main argv =
        let instructions = readFile argv.[0]

        let decoderChipV1 = 
            {
                SeaPortV1.DecoderChip.Memory = new Dictionary<uint64, uint64>()
                SeaPortV1.DecoderChip.Mask = List.Empty
            }

        decoderChipV1.Execute instructions

        Seq.sum decoderChipV1.Memory.Values
        |> printfn "The answer to part 1 is '%d'."

        let decoderChipV2 = 
            {
                SeaPortV2.DecoderChip.Memory = new Dictionary<uint64, uint64>()
                SeaPortV2.DecoderChip.Mask = List.Empty
            }

        decoderChipV2.Execute instructions

        Seq.sum decoderChipV2.Memory.Values
        |> printfn "The answer to part 2 is '%d'."

        0