namespace AdventOfCode2020.Day14

open System.Collections.Generic
open AdventOfCode2020.Day14.Instruction

module SeaPortV1 =

    type DecoderChip = 
        {
            Memory:Dictionary<uint64, uint64>
            mutable Mask:list<char>
        }

        member private this.ApplyMask input =
            let mutable output = input

            for i in 0 .. this.Mask.Length - 1 do
                match this.Mask.[i] with
                | '1' -> output <- output ||| (1UL <<< (this.Mask.Length - 1 - i))
                | '0' -> output <- output &&& ~~~ (1UL <<< (this.Mask.Length - 1 - i))
                | _ -> ()

            output

        member private this.WriteValue address value = 
            this.Memory.Remove(address) |> ignore
            this.Memory.Add(address, value)

        member this.Execute instructions = 
            for instruction in instructions do
                match instruction with
                | SetMask(value) -> this.Mask <- value
                | WriteToMem(address, value) -> this.ApplyMask value |> this.WriteValue address

module SeaPortV2 =

    let rec private applyMask mask input = 
        if List.isEmpty mask then
            [
                input
            ]
        else
            [
                let i = List.length mask - 1 
                let tails = applyMask (List.tail mask) input

                for tail in tails do
                    match List.head mask with
                    | '1' -> yield tail ||| (1UL <<< i)
                    | 'X' -> yield tail ||| (1UL <<< i); yield tail &&& ~~~ (1UL <<< i)
                    | _ -> yield tail
            ]
    
    type DecoderChip = 
        {
            Memory:Dictionary<uint64, uint64>
            mutable Mask:list<char>
        }

        member private this.ApplyMask input = applyMask this.Mask input
            

        member private this.WriteValue address value =
            let addresses = this.ApplyMask address
            List.iter (fun address -> this.Memory.Remove(address) |> ignore; this.Memory.Add(address, value)) addresses

        member this.Execute instructions =
            for instruction in instructions do
                match instruction with
                | SetMask(value) -> this.Mask <- value
                | WriteToMem(address, value) -> this.WriteValue address value