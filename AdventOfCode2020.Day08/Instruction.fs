namespace AdventOfCode2020.Day08

open System.Text.RegularExpressions

module Instruction = 

    type Instruction = {
        mutable Operation:string
        Value:int
        mutable HasExecuted:bool
    }

    let parseInstruction text =
        let pattern = @"^([a-z]{3}) \+?(\-?\d+)$"
        let m = Regex.Match(text, pattern)

        if m.Success then
            {
                Operation = m.Groups.Item(1).Value
                Value = int(m.Groups.Item(2).Value)
                HasExecuted = false
            }
        else
            failwithf "Invalid instruction '%s'" text

    let parseInstructionSet lines =
        [
            for line in lines do
                yield parseInstruction line
        ]

    let executeInstructionSet throw (instructionSet:list<Instruction>) =
        let mutable pointer = 0
        let mutable continueLoop = true
        let mutable accumulator = 0

        while continueLoop do
            let instruction = instructionSet.Item(pointer)

            if not instruction.HasExecuted then
            
                match instruction.Operation with
                | "acc" -> accumulator <- accumulator + instruction.Value; pointer <- pointer + 1
                | "jmp" -> pointer <- pointer + instruction.Value
                | "nop" -> pointer <- pointer + 1
                | _ -> failwithf "Unsupported operation '%s'" instruction.Operation
                
                instruction.HasExecuted <- true
                continueLoop <- pointer < instructionSet.Length
            else if not throw then
                continueLoop <- false
            else
                failwithf "Infinite loop detected at position %d" pointer
        
        accumulator

    let copyInstructionSet instructionSet = 
        [
            for instruction in instructionSet do
                yield {
                    Operation = instruction.Operation
                    Value = instruction.Value
                    HasExecuted = instruction.HasExecuted
                }
        ]

    let modifyOperation instruction = 
        match instruction.Operation with
        | "nop" -> instruction.Operation <- "jmp"
        | "jmp" -> instruction.Operation <- "nop"
        | _ -> ()

    let buildAlternateInstructionSet (instructionSet:list<Instruction>, indexToModify) =
        let copyOfInstructions = copyInstructionSet instructionSet
        let instruction = copyOfInstructions.Item(indexToModify)
        modifyOperation instruction
        copyOfInstructions

    let buildAlternateInstructionSets (instructionSet:list<Instruction>) =
        seq {
            for i in 0 .. instructionSet.Length - 1 do
                match instructionSet.Item(i).Operation with
                | "nop" | "jmp" -> yield buildAlternateInstructionSet (instructionSet, i)
                | _ -> ()
        }

    let testInstructionSet (instructionSet:list<Instruction>) =
        try
            (true, executeInstructionSet true instructionSet)
        with
            | _ -> (false, 0)

    let testInstructionSets (instructionSets:seq<list<Instruction>>) = 
        Seq.map testInstructionSet instructionSets
        |> Seq.filter (fun result -> fst(result))
        |> Seq.head
        |> snd