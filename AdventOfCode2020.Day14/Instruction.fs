namespace AdventOfCode2020.Day14

module Instruction =

    type Instruction =
        | SetMask of Value:list<char>
        | WriteToMem of Address:uint64 * Value:uint64