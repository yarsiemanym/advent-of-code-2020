module AdventOfCode2020.Day02

open System.IO
open System.Text.RegularExpressions

type Line = 
    { 
        Min:int
        Max:int
        Character:char
        Password:string
    }

let parseLines lines =
    let pattern = @"^(\d+)-(\d+)\s+([a-z]):\s+([a-z]+)$"
    seq {
        for line in lines do
            let m = Regex.Match(line, pattern)

            if m.Success then 
                yield { 
                    Min = int(m.Groups.Item(1).Value)
                    Max = int(m.Groups.Item(2).Value)
                    Character = m.Groups.Item(3).Value.[0]
                    Password = m.Groups.Item(4).Value
                } 
            else
                printfn "Invalid line '%s'" line
    }

let readFile =
    File.ReadAllLines 
    >> parseLines

let countInstanceOf x =
    Seq.filter ((=) x) 
    >> Seq.length

let isBetween min max value = min <= value && value <= max

let charAtIndexIsEqualTo (i, s:string, c) = s.[i - 1] = c

let isValid line = 
    // countInstanceOf line.Character line.Password |> isBetween line.Min line.Max
    charAtIndexIsEqualTo(line.Min, line.Password, line.Character) <> charAtIndexIsEqualTo(line.Max, line.Password, line.Character)

let findValidPasswords lines = 
    seq {
        for line in lines do
            if isValid line then 
                yield line.Password
    }

let printAnswer answer = printfn "The answer is '%d'." answer

let findAnswer = 
    readFile
    >> findValidPasswords
    >> Seq.length
    >> printAnswer

[<EntryPoint>]
let main argv =
    findAnswer argv.[0]
    0