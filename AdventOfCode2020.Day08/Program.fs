module AdventOfCode2020.Day08

open System
open System.IO
open System.Text.RegularExpressions

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
        raise (Exception (sprintf "Invalid instruction '%s'" text))

let parseInstructionSet lines =
    [
        for line in lines do
            yield parseInstruction line
    ]

let readFile =
    File.ReadAllLines
    >> parseInstructionSet

let executeInstructionSet (instructionSet:list<Instruction>) =
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
            | _ -> raise (Exception (sprintf "Unsupported operation '%s'" instruction.Operation))
            
            instruction.HasExecuted <- true
            continueLoop <- pointer < instructionSet.Length
        else
            raise (Exception (sprintf "Infinite loop detected at position %d" pointer))
    
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

let buildModifiedInstructionSet (instructionSet:list<Instruction>, indexToModify) =
    let copyOfInstructions = copyInstructionSet instructionSet
    let instruction = copyOfInstructions.Item(indexToModify)
    modifyOperation instruction
    copyOfInstructions

let buildModifiedInstructionSets (instructionSet:list<Instruction>) =
    seq {
        for i in 0 .. instructionSet.Length - 1 do
            buildModifiedInstructionSet (instructionSet, i)
    }

let testModifiedInstructionSet (instructionSet:list<Instruction>) =
    try
        (true, executeInstructionSet instructionSet)
    with
        | ex -> (false, 0)

let testModifiedInstructionSets (instructionSets:seq<list<Instruction>>) = 
    Seq.map (testModifiedInstructionSet) instructionSets
    |> Seq.filter (fun result -> fst(result))
    |> Seq.head
    |> snd

let printAnswer answer = printfn "The answer is '%d'." answer

let findAnswer = 
    readFile
    >> buildModifiedInstructionSets
    >> testModifiedInstructionSets
    >> printAnswer

[<EntryPoint>]
let main argv =
    findAnswer argv.[0]
    0
