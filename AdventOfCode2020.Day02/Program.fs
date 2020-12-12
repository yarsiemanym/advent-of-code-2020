namespace AdventOfCode2020.Day02

open System.IO
open System.Text.RegularExpressions

module Program =

    type Line = 
        { 
            Min:int
            Max:int
            Character:char
            Password:string
        }

    let parseLines lines =
        let pattern = @"^(\d+)-(\d+)\s+([a-z]):\s+([a-z]+)$"
        
        [
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
                    failwithf "Invalid line '%s'" line
        ]

    let readFile =
        File.ReadAllLines 
        >> parseLines

    let countInstanceOf x =
        Seq.toList
        >> List.filter ((=) x) 
        >> List.length

    let isBetween min max value = min <= value && value <= max

    let charAtIndexIsEqualTo (i, s:string, c) = s.[i - 1] = c

    let isValidPart1 line = countInstanceOf line.Character line.Password |> isBetween line.Min line.Max

    let isValidPart2 line = charAtIndexIsEqualTo(line.Min, line.Password, line.Character) <> charAtIndexIsEqualTo(line.Max, line.Password, line.Character)

    let validatePasswords lines validator = 
        [
            for line in lines do
                yield validator line
        ]

    [<EntryPoint>]
    let main argv =
        let lines = readFile argv.[0]
        
        validatePasswords lines isValidPart1
        |> List.filter ((=) true)
        |> List.length
        |> printfn "The answer to part 1 is '%d'."

        validatePasswords lines isValidPart2
        |> List.filter ((=) true)
        |> List.length
        |> printfn "The answer to part 2 is '%d'."

        0