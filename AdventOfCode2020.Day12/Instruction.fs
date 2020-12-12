namespace AdventOfCode2020.Day12

module Instruction =

    type Instruction =
        {
            Operation:char
            Value:int
        }

    let parseInstruction (text:string) = { Operation = text.[0]; Value = int text.[1..] }

    let parseInstructions lines = [ for line in lines do yield parseInstruction line ]