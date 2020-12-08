module AdventOfCode2020.Day08

open System
open System.IO
open System.Text.RegularExpressions

type Instruction = {
    Operation:string
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

let parseInstructions lines =
    [
        for line in lines do
            yield parseInstruction line
    ]

let readFile =
    File.ReadAllLines
    >> parseInstructions

let executeInstructions (instructions:list<Instruction>) =
    let mutable pointer = 0
    let mutable continueLoop = true
    let mutable accumulator = 0

    while continueLoop do
        let instruction = instructions.Item(pointer)

        if not instruction.HasExecuted then
        
            match instruction.Operation with
            | "acc" -> accumulator <- accumulator + instruction.Value; pointer <- pointer + 1
            | "jmp" -> pointer <- pointer + instruction.Value
            | "nop" -> pointer <- pointer + 1
            | _ -> raise (Exception (sprintf "Unsupported operation '%s'" instruction.Operation))
            
            instruction.HasExecuted <- true
            continueLoop <- pointer < instructions.Length
        else
            printfn "Infinite loop detected at position %d" pointer
            continueLoop <- false
    
    accumulator

let printAnswer answer = printfn "The answer is '%d'." answer

let findAnswer = 
    readFile
    >> executeInstructions
    >> printAnswer

[<EntryPoint>]
let main argv =
    findAnswer argv.[0]
    0
